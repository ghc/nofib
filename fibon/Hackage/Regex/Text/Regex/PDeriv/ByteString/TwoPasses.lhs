> {- By Kenny Zhuo Ming Lu and Martin Sulzmann, 2009, BSD License -}

A bytestring implementation of reg exp pattern matching using partial derivative
This algorithm exploits the extension of partial derivative of regular expression patterns.
This algorithm starts by scanning the input words from right to left. The goal is 
for each prefix of the input word, we keep track of the set of "successful" states that lead to 
a successful match. Then We perform the actual matching by scanning the words
from left to right. Making use of the sets of "successful" states, we prune away 
failures states as long as we cannot find them in the sets.

> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
>     FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 


> module Text.Regex.PDeriv.ByteString.TwoPasses
>     ( Regex
>     , CompOption(..)
>     , ExecOption(..)
>     , defaultCompOpt
>     , defaultExecOpt
>     , compile
>     , execute
>     , regexec
>     ) where 

> import Data.List 
> import Data.Char (ord)
> import GHC.Int
> import qualified Data.IntMap as IM
> import qualified Data.ByteString.Char8 as S

> import Text.Regex.Base(RegexOptions(..))

> import Text.Regex.PDeriv.RE
> import Text.Regex.PDeriv.Pretty (Pretty(..))
> import Text.Regex.PDeriv.Common (Range, Letter, IsEmpty(..), my_hash, my_lookup, GFlag(..), IsGreedy(..), nub2)
> import Text.Regex.PDeriv.IntPattern (Pat(..), pdPat, pdPat0, toBinder, Binder(..), strip, listifyBinder)
> import Text.Regex.PDeriv.Parse
> import qualified Text.Regex.PDeriv.Dictionary as D (Dictionary(..), Key(..), insertNotOverwrite, lookupAll, empty, isIn, nub)

A word is a byte string.

> type Word = S.ByteString



version using the SNFA

In addtion, we pass in a lookup table that maps
target_state * letter to source_state.

> rev_scanIntState :: [Int] -- ^ the final states
>                     -> PdPat0TableRev -- ^ the reverse mapping table
>                     -> Word 
>                     -> [[Int]]
> rev_scanIntState sfinal table w = table `seq` rev_scan_helperIntState (S.reverse w) table sfinal []

> rev_scan_helperIntState w table curr_states chain = 
>     case S.uncons w of 
>     Just (l,w') ->
>           let 
>                 pairs  =     [ (p_s,p_t) | p_t <- curr_states, 
>                                p_s <- my_lookup p_t l table ]
>                 (next_states', curr_states') = unzip pairs
>                 next_states'' =  nub next_states'
>                 curr_states'' =  nub curr_states'
>                 next_chain = curr_states'':chain
>           in (rev_scan_helperIntState w' table $! next_states'') $! next_chain
>     _ -> curr_states:chain




----------------------------
-- (greedy) pattern matching

> type Env = [(Int,Word)]



> rg_collect :: S.ByteString -> (Int,Int) -> S.ByteString
> rg_collect w (i,j) = S.take (j' - i' + 1) (S.drop i' w)
>	       where i' = fromIntegral i
>	             j' = fromIntegral j



actual pattern matcher

we compile all the possible partial derivative operation into a table
The table maps key to a set of target integer states and their corresponding
binder update functions. 

> type PdPat0Table = IM.IntMap [(Int, Int -> Binder -> Binder)]

we also record the reverse transition in another hash table, which will be used in the reverse scanning process

> type PdPat0TableRev = IM.IntMap [Int] 

> -- | function 'buildPdPat0Table' builds the above tables from the pattern


> buildPdPat0Table :: Pat ->  (PdPat0Table, [Int], PdPat0TableRev)
> buildPdPat0Table init = 
>     let sig = map (\x -> (x,0)) (sigmaRE (strip init))         --  the sigma
>         init_dict = D.insertNotOverwrite (D.hash init) (init,0) D.empty         --  add init into the initial dictionary
>         (all, delta, dictionary) = sig `seq` builder sig [] [] [init] init_dict 1   --  all states and delta
>         final = all `seq`  [ s | s <- all, isEmpty (strip s)]                   --  the final states
>         sfinal = final `seq` dictionary `seq` map (mapping dictionary) final
>         sdelta = [ (i,l,jfs) | 
>                   (p,l, qfs) <- delta, 
>                   let i = mapping dictionary p
>                       jfs = map (\(q,f) -> (mapping dictionary q, f)) qfs
>                   ]
>         hash_table = foldl (\ dict (p,x,q) -> 
>                                  let k = my_hash p (fst x)
>                                  in case IM.lookup k dict of 
>                                       Just ps -> error "Found a duplicate key in the PdPat0Table, this should not happend."
>                                       Nothing -> IM.insert k q dict) IM.empty sdelta
>         sdelta_rev = [ (j,l,i) | 
>                        (p,l, qfs) <- delta, 
>                        let i = mapping dictionary p
>                            jfs = map (\(q,f) -> (mapping dictionary q, f)) qfs,
>                        (j,f) <- jfs
>                      ]
>         hash_table_rev = foldl (\ dict (p,x,q) -> 
>                                  let k = my_hash p (fst x)
>                                  in case IM.lookup k dict of 
>                                       Just ps -> IM.update (\ _ -> Just (q:ps)) k dict
>                                       Nothing -> IM.insert k [q] dict) IM.empty sdelta_rev
>     in (hash_table, sfinal, hash_table_rev)

Some helper functions used in buildPdPat0Table

> myLookup = lookup

> mapping :: D.Dictionary (Pat,Int) -> Pat -> Int
> mapping dictionary x = let candidates = D.lookupAll (D.hash x) dictionary
>                        in candidates `seq` 
>                           case candidates of
>                             [(_,i)] -> i
>                             _ -> 
>                                 case myLookup x candidates of
>                                 (Just i) -> i
>                                 Nothing -> error ("this should not happen. looking up " ++ (pretty x) ++ " from " ++ (show candidates) )

> builder :: [Letter] 
>         -> [Pat] 
>         -> [(Pat,Letter, [(Pat, Int -> Binder -> Binder)] )]
>         -> [Pat] 
>         -> D.Dictionary (Pat,Int)
>         -> Int 
>         -> ([Pat], [(Pat, Letter, [(Pat, Int -> Binder -> Binder)])], D.Dictionary (Pat,Int))
> builder sig acc_states acc_delta curr_states dict max_id 
>     | null curr_states  = (acc_states, acc_delta, dict)
>     | otherwise = 
>         let 
>             all_sofar_states = acc_states ++ curr_states
>             new_delta = [ (s, l, sfs) | s <- curr_states, l <- sig, let sfs = pdPat0 s l]
>             new_states = all_sofar_states `seq` D.nub [ s' | (_,_,sfs) <- new_delta, (s',f) <- sfs
>                                                       , not (s' `D.isIn` dict) ]
>             acc_delta_next  = (acc_delta ++ new_delta)
>             (dict',max_id') = new_states `seq` foldl (\(d,id) p -> (D.insertNotOverwrite (D.hash p) (p,id) d, id + 1) ) (dict,max_id) new_states
>         in {- dict' `seq` max_id' `seq` -} builder sig all_sofar_states acc_delta_next new_states dict' max_id' 




> -- | function 'lookupPdPat0' looks up the "partial derivative" operations given an integer state and apply the update function to the binder.

> lookupPdPat0 :: PdPat0Table -> (Int,Binder) -> Letter -> [(Int,Binder)]
> lookupPdPat0 hash_table (i,binder) (l,x) = 
>     case IM.lookup (my_hash i l) hash_table of
>     Just pairs -> 
>         [ (j, op x binder) | (j, op) <- pairs ]
>     Nothing -> []

> -- | function 'collectPatMatchFromBinder' collects match results from binders
> collectPatMatchFromBinder :: Word -> Binder -> Env
> collectPatMatchFromBinder w b = collectPatMatchFromBinder_ w (listifyBinder b)
> collectPatMatchFromBinder_ w [] = []
> collectPatMatchFromBinder_ w ((x,[]):xs) = (x,S.empty):(collectPatMatchFromBinder_ w xs)
> collectPatMatchFromBinder_ w ((x,rs):xs) = (x,foldl S.append S.empty $ map (rg_collect w) (reverse rs)):(collectPatMatchFromBinder_ w xs)


> -- | 'patMAtchIntStatePdPat0' implements the two passes pattern matching algo
> patMatchIntStatePdPat0 :: Pat -> Word -> [Env]
> patMatchIntStatePdPat0 p w = 
>   let
>     (pdStateTable,sfinals,pdStateTableRev) = buildPdPat0Table p
>     filters = pdStateTableRev `seq` sfinals `seq` rev_scanIntState sfinals pdStateTableRev w
>     s = 0
>     b = toBinder p
>     allbinders' = b `seq` s `seq` pdStateTable `seq` filters `seq` (patMatchesIntStatePdPat0 0 pdStateTable w [(s,b)]) filters
>     allbinders = allbinders' `seq` map snd allbinders'
>   in map (collectPatMatchFromBinder w) $! allbinders

> patMatchesIntStatePdPat0 :: Int -> PdPat0Table -> Word -> [(Int,Binder)] -> [[Int]] -> [(Int,Binder)]
> patMatchesIntStatePdPat0 cnt pdStateTable  w' ps fps' =
>     case (S.uncons w', fps') of 
>       (Nothing,_) -> ps 
>       (Just (l,w),(fp:fps)) -> 
>           let 
>               reachable_ps = ps `seq` fp `seq` nub2 [ p | p@(s,_) <- ps, elem s fp ]
>               next_ps = l `seq` cnt `seq` pdStateTable `seq` reachable_ps `seq` concat [ lookupPdPat0 pdStateTable  p (l,cnt) | p <- reachable_ps ]
>               cnt' = cnt + 1
>          in cnt' `seq` pdStateTable `seq` w `seq` next_ps `seq` fps `seq` patMatchesIntStatePdPat0 cnt'  pdStateTable  w  next_ps fps
>       (Just (l,w),[]) ->
>           error "patMatchesIntStatePdPat0 failed with empty fps and non empty input word!"


> greedyPatMatch :: Pat -> Word -> Maybe Env
> greedyPatMatch p w =
>      first (patMatchIntStatePdPat0 p w)
>   where
>     first (env:_) = return env
>     first _ = Nothing
>  

> -- | Compilation
> compilePat :: Pat -> (PdPat0Table, [Int], PdPat0TableRev, Binder)
> compilePat p =  pdStateTable `seq` pdStateTableRev `seq` sfinal `seq` (pdStateTable, sfinal, pdStateTableRev, b)
>     where 
>           (pdStateTable,sfinal,pdStateTableRev) = buildPdPat0Table p
>           b = toBinder p

> patMatchIntStateCompiled :: (PdPat0Table, [Int], PdPat0TableRev, Binder) -> Word -> [Env]
> patMatchIntStateCompiled (pdStateTable,sfinals, pdStateTableRev, b) w = 
>   let
>     filters = sfinals `seq` pdStateTableRev `seq` rev_scanIntState sfinals pdStateTableRev $! w 
>     s = 0 
>     allbinders' = b `seq` s `seq` pdStateTable `seq` filters `seq` (patMatchesIntStatePdPat0 0 pdStateTable w [(s,b)]) filters
>     allbinders =  map snd allbinders'
>   in map (collectPatMatchFromBinder w) allbinders



> greedyPatMatchCompiled :: (PdPat0Table, [Int], PdPat0TableRev, Binder)  -> Word -> Maybe Env
> greedyPatMatchCompiled compiled w =
>      first (patMatchIntStateCompiled compiled w)
>   where
>     first (env:_) = return env
>     first _ = Nothing




> newtype Regex = Regex (PdPat0Table, [Int], PdPat0TableRev, Binder)  


-- todo: use the CompOption and ExecOption

> compile :: CompOption -- ^ Flags (summed together)
>         -> ExecOption -- ^ Flags (summed together) 
>         -> S.ByteString -- ^ The regular expression to compile
>         -> Either String Regex -- ^ Returns: the compiled regular expression
> compile compOpt execOpt bs =
>     case parsePat (S.unpack bs) of
>     Left err -> Left ("parseRegex for Text.Regex.PDeriv.ByteString failed:"++show err)
>     Right pat -> Right (patToRegex pat compOpt execOpt)
>     where 
>       patToRegex p _ _ = Regex (compilePat p)



> execute :: Regex      -- ^ Compiled regular expression
>        -> S.ByteString -- ^ ByteString to match against
>        -> Either String (Maybe Env)
> execute (Regex r) bs = Right (greedyPatMatchCompiled r bs)

> regexec :: Regex      -- ^ Compiled regular expression
>        -> S.ByteString -- ^ ByteString to match against
>        -> Either String (Maybe (S.ByteString, S.ByteString, S.ByteString, [S.ByteString]))
> regexec (Regex r) bs =
>  case greedyPatMatchCompiled r bs of
>    Nothing -> Right (Nothing)
>    Just env ->
>      let pre = case lookup (-1) env of { Just w -> w ; Nothing -> S.empty }
>          post = case lookup (-2) env of { Just w -> w ; Nothing -> S.empty }
>          full_len = S.length bs
>          pre_len = S.length pre
>          post_len = S.length post
>          main_len = full_len - pre_len - post_len
>          main_and_post = S.drop pre_len bs
>          main = main_and_post `seq` main_len `seq` S.take main_len main_and_post
>          matched = map snd (filter (\(v,w) -> v > 0) env)
>      in Right (Just (pre,main,post,matched))


> -- | Control whether the pattern is multiline or case-sensitive like Text.Regex and whether to
> -- capture the subgroups (\1, \2, etc).  Controls enabling extra anchor syntax.
> data CompOption = CompOption {
>       caseSensitive :: Bool    -- ^ True in blankCompOpt and defaultCompOpt
>     , multiline :: Bool 
>   {- ^ False in blankCompOpt, True in defaultCompOpt. Compile for
>   newline-sensitive matching.  "By default, newline is a completely ordinary
>   character with no special meaning in either REs or strings.  With this flag,
>   inverted bracket expressions and . never match newline, a ^ anchor matches the
>   null string after any newline in the string in addition to its normal
>   function, and the $ anchor matches the null string before any newline in the
>   string in addition to its normal function." -}
>     , rightAssoc :: Bool       -- ^ True (and therefore Right associative) in blankCompOpt and defaultCompOpt
>     , newSyntax :: Bool        -- ^ False in blankCompOpt, True in defaultCompOpt. Add the extended non-POSIX syntax described in "Text.Regex.TDFA" haddock documentation.
>     , lastStarGreedy ::  Bool  -- ^ False by default.  This is POSIX correct but it takes space and is slower.
>                                -- Setting this to true will improve performance, and should be done
>                                -- if you plan to set the captureGroups execoption to False.
>     } deriving (Read,Show)

> data ExecOption = ExecOption  {
>   captureGroups :: Bool    -- ^ True by default.  Set to False to improve speed (and space).
>   } deriving (Read,Show)

> instance RegexOptions Regex CompOption ExecOption where
>     blankCompOpt =  CompOption { caseSensitive = True
>                                , multiline = False
>                                , rightAssoc = True
>                                , newSyntax = False
>                                , lastStarGreedy = False
>                                  }
>     blankExecOpt =  ExecOption { captureGroups = True }
>     defaultCompOpt = CompOption { caseSensitive = True
>                                 , multiline = True
>                                 , rightAssoc = True
>                                 , newSyntax = True
>                                 , lastStarGreedy = False
>                                   }
>     defaultExecOpt =  ExecOption { captureGroups = True }
>     setExecOpts e r = undefined
>     getExecOpts r = undefined 



-- Kenny's example

> long_pat = PPair (PVar 1 [] (PE (Star (L 'A') Greedy))) (PVar 2 [] (PE (Star (L 'A') Greedy)))
> long_string n = S.pack $ (take 0 (repeat 'A')) ++ (take n (repeat 'B'))

-- p4 = << x : (A|<A,B>), y : (<B,<A,A>>|A) >, z : (<A,C>|C) > 

> p4 = PPair (PPair p_x p_y) p_z
>    where p_x = PVar 1 [] (PE (Choice (L 'A') (Seq (L 'A') (L 'B')) Greedy))      
>          p_y = PVar 2 [] (PE (Choice (Seq (L 'B') (Seq (L 'A') (L 'A'))) (L 'A') Greedy))
>          p_z = PVar 3 [] (PE (Choice (Seq (L 'A') (L 'C')) (L 'C') Greedy))

> input = S.pack "ABAAC"  -- long(posix) vs greedy match


> p5 = PStar (PVar 1 [] (PE (Choice (L 'A') (Choice (L 'B') (L 'C') Greedy) Greedy))) Greedy

pattern = ( x :: (A|C), y :: (B|()) )*

> p6 = PStar (PPair (PVar 1 [] (PE (Choice (L 'A') (L 'C') Greedy))) (PVar 2 [] (PE (Choice (L 'B') Empty Greedy)))) Greedy

pattern = ( x :: ( y :: A, z :: B )* )

> p7 = PVar 1 [] (PStar (PPair (PVar 2 [] (PE (L 'A'))) (PVar 3 [] (PE (L 'B')))) Greedy)

> input7 = S.pack "ABABAB"


pattern = ( x :: A*?, y :: A*)

> p8 = PPair (PVar 1 [] (PE (Star (L 'A') NotGreedy))) (PVar 2 [] (PE (Star (L 'A') Greedy)))

> input8 = S.pack "AAAAAA"

pattern = ( x :: A*?, y :: A*)

> p9 = PPair (PStar (PVar 1 [] (PE (L 'A'))) NotGreedy) (PVar 2 [] (PE (Star (L 'A') Greedy)))

pattern = ( x :: (A|B)*?, (y :: (B*,A*)))

> p10 = PPair (PVar 1 [] (PE (Star (Choice (L 'A') (L 'B') Greedy) NotGreedy))) (PVar 2 [] (PE (Seq (Star (L 'B') Greedy) (Star (L 'A') Greedy))))

> input10 = S.pack "ABA"


pattern = <(x :: (0|...|9)+?)*, (y :: (0|...|9)+?)*, (z :: (0|...|9)+?)*>

> digits_re = foldl (\x y -> Choice x y Greedy) (L '0') (map L "12345789")

> p11 = PPair (PStar (PVar 1 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy) (PPair (PStar (PVar 2 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy) (PPair (PStar (PVar 3 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy) (PStar (PVar 4 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy)))

> input11 = S.pack "1234567890123456789-"