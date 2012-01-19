> {- By Kenny Zhuo Ming Lu and Martin Sulzmann, 2009, BSD License -}

A bytestring implementation of reg exp pattern matching using partial derivative
This algorithm exploits the extension of partial derivative of regular expression patterns.
This algorithm proceeds by scanning the input word from left to right until we reach 
an emptiable pattern and the input word is fully consumed.

> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
>     FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 


> module Text.Regex.PDeriv.ByteString.LeftToRight
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
> -- import GHC.Int
> import qualified Data.IntMap as IM
> import qualified Data.ByteString.Char8 as S
> import Control.DeepSeq

> import System.IO.Unsafe (unsafePerformIO)

> import Text.Regex.Base(RegexOptions(..))


> import Text.Regex.PDeriv.RE
> import Text.Regex.PDeriv.Pretty (Pretty(..))
> import Text.Regex.PDeriv.Common (Range, Letter, IsEmpty(..), my_hash, my_lookup, GFlag(..), IsEmpty(..), nub2)
> import Text.Regex.PDeriv.IntPattern (Pat(..), pdPat, pdPat0, toBinder, Binder(..), strip, listifyBinder)
> import Text.Regex.PDeriv.Parse
> import qualified Text.Regex.PDeriv.Dictionary as D (Dictionary(..), Key(..), insertNotOverwrite, lookupAll, empty, isIn, nub)



A word is a byte string.

> type Word = S.ByteString


----------------------------
-- (greedy) pattern matching

> type Env = [(Int,Word)]

> rg_collect :: S.ByteString -> (Int,Int) -> S.ByteString
> rg_collect w (i,j) = S.take (j' - i' + 1) (S.drop i' w)
>	       where i' = fromIntegral i
>	             j' = fromIntegral j



we compile all the possible partial derivative operation into a table
The table maps key to a set of target integer states and their corresponding
binder update functions. 

> type PdPat0Table = IM.IntMap [(Int, Int -> Binder -> Binder)]

A function that builds the above table from the pattern

> buildPdPat0Table :: Pat ->  (PdPat0Table, [Int])
> buildPdPat0Table init = 
>     let sig = map (\x -> (x,0)) (sigmaRE (strip init))         -- the sigma
>         init_dict = D.insertNotOverwrite (D.hash init) (init,0) D.empty         -- add init into the initial dictionary
>         (all, delta, dictionary) = sig `seq` builder sig [] [] [init] init_dict 1   -- all states and delta
>         final = all `seq`  [ s | s <- all, isEmpty (strip s)]                   -- the final states
>         sfinal = final `seq` dictionary `seq` map (mapping dictionary) final
>         lists = [ (i,l,jfs) | 
>                   (p,l, qfs) <- delta, 
>                   let i = mapping dictionary p
>                       jfs = map (\(q,f) -> (mapping dictionary q, f)) qfs
>                   ]
>         hash_table = foldl' (\ dict (p,x,q) -> 
>                                  let k = my_hash p (fst x)
>                                  in case IM.lookup k dict of 
>                                       Just ps -> error "Found a duplicate key in the PdPat0Table, this should not happen."
>                                       Nothing -> IM.insert k q dict) IM.empty lists
>     in (hash_table, sfinal)

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
>             (dict',max_id') = new_states `seq` foldl' (\(d,id) p -> (D.insertNotOverwrite (D.hash p) (p,id) d, id + 1) ) (dict,max_id) new_states
>         in {- dict' `seq` max_id' `seq` -} builder sig all_sofar_states acc_delta_next new_states dict' max_id' 



the "partial derivative" operations among integer states + binders

> lookupPdPat0 :: PdPat0Table -> (Int,Binder) -> Letter -> [(Int,Binder)]
> lookupPdPat0 hash_table (i,binder) (l,x) = 
>     -- i `seq` 
>     -- l `seq` 
>     -- k `seq` 
>     let  k =  {-# SCC "hash" #-} (my_hash i l)
>     in k `seq`
>     hash_table `seq`
>       case {-# SCC "lookup" #-} IM.lookup k hash_table of
>       { Just pairs -> 
>             binder `seq` -- x `seq`
>         -- {-# SCC "pair" #-} [ binder' `seq`  (j, binder' ) | (j, op) <- {-# SCC "pair_pair" #-} pairs, let binder' = {-# SCC "pair_binder" #-} op x binder ]
>         {-# SCC "pair" #-} map (\ (j,op) -> let binder' = {-# SCC "pair_binder" #-} op x binder  
>                                             in binder' `seq`  
>                                 {-# SCC "pair_pair" #-} (j, binder' ) ) pairs  
>       ; Nothing -> [] 
>       }


> lookupPdPat0' :: PdPat0Table -> (Int, [Binder -> Binder]) -> Letter -> [(Int,[Binder -> Binder])]
> lookupPdPat0' hash_table (i,fs) (l,x) = 
>     -- i `seq` 
>     -- l `seq` 
>     -- k `seq` 
>     let  k =  {-# SCC "hash" #-} (my_hash i l)
>     in k `seq`
>     hash_table `seq`
>       case {-# SCC "lookup" #-} IM.lookup k hash_table of
>       { Just pairs -> 
>             let io = unsafePerformIO (print (length pairs))
>             in
>             x `seq` -- io `seq`
>         {-# SCC "pair" #-} map (\ (j,op) -> let f = {-# SCC "op_x" #-} op x 
>                                                 fs' = {-# SCC "fs'" #-} {- f `seq` fs `seq` -} f:fs
>                                             in {- fs' `seq` -} (j, fs')) pairs 
>       ; Nothing -> [] 
>       }


collection function for binder 

> collectPatMatchFromBinder :: Word -> Binder -> Env
> collectPatMatchFromBinder w b = 
>     collectPatMatchFromBinder_ w (listifyBinder b)

> collectPatMatchFromBinder_ w [] = []
> collectPatMatchFromBinder_ w ((x,[]):xs) = (x,S.empty):(collectPatMatchFromBinder_ w xs)
> collectPatMatchFromBinder_ w ((x,rs):xs) = (x,foldl' S.append S.empty $ map (rg_collect w) (reverse rs)):(collectPatMatchFromBinder_ w xs)
> {-
>                                            (x, f w rs):(collectPatMatchFromBinder_ w xs)
>     where f w [] = S.empty
>           f w (r:_) = rg_collect w r
> -}

> patMatchesIntStatePdPat0 :: Int -> PdPat0Table -> Word -> [(Int,Binder)] -> [(Int,Binder)]
> patMatchesIntStatePdPat0 cnt pdStateTable  w' eps =
>     case {-# SCC "uncons" #-} S.uncons w' of 
>       Nothing -> eps 
>       Just (l,w) -> 
>           let 
>               eps_ = -- l `seq` cnt `seq` 
>                      {-# SCC "listcompred"  #-} concatMap (\ep -> lookupPdPat0 pdStateTable ep (l,cnt)) eps
>               eps' = -- eps_ `seq`
>                      nub2 eps_
>               cnt' = cnt + 1
>           in   cnt' `seq` {- pdStateTable `seq` -} w `seq` 
>                eps' `seq` 
>                patMatchesIntStatePdPat0 cnt'  pdStateTable  w eps'


> patMatchesIntStatePdPat0' :: Int -> PdPat0Table -> Word -> [(Int,[Binder -> Binder])] -> [(Int,[Binder -> Binder])]
> patMatchesIntStatePdPat0' cnt pdStateTable  w' eps =
>     case {-# SCC "uncons" #-} S.uncons w' of 
>       Nothing -> eps 
>       Just (l,w) -> 
>           let 
>               eps_ = l `seq` cnt `seq` 
>                      {-# SCC "listcompred" #-} concatMap (\ep -> lookupPdPat0' pdStateTable ep (l,cnt)) eps
>               eps' = -- eps_ `seq`
>                      nub2 eps_
>               cnt' = cnt + 1
>           in   cnt' `seq` {- pdStateTable `seq` -} w `seq` 
>                eps' `seq` 
>                patMatchesIntStatePdPat0' cnt' pdStateTable  w eps'

> concatMap' :: (a -> [b]) -> [a] -> [b]
> concatMap' f x = foldr' ( \ b a -> (++) a (f b) ) [] x

> foldr' :: (a -> b -> b) -> b -> [a] -> b
> foldr' f b [] = b
> foldr' f b (a:as) = let b' = f a b 
>                     in b' `seq` 
>                        foldr' f b' as

> {- 
> fast_nub :: [(Binder,Int)] -> [(Binder,Int)]
> fast_nub eps = 
>     let im = IM.empty 
>     in fast_nub' im eps
>     where fast_nub' :: IM.IntMap () -> [(Binder,Int)] -> [(Binder,Int)]
>           fast_nub' im [] = []
>           fast_nub' im ((e,p):eps) = 
>               let mb_r = IM.lookup p im
>               in case mb_r of
>                  Just _ ->  fast_nub' im eps
>                  Nothing -> let im' = IM.insert p () im
>                             in (e,p):(fast_nub' im' eps)
> -}


> patMatchIntStatePdPat0 :: Pat -> Word -> [Env]
> patMatchIntStatePdPat0 p w = 
>   let
>     (pdStateTable,sfinal) = buildPdPat0Table p
>     s = 0
>     b = toBinder p
>     allbinders' = b `seq` s `seq` pdStateTable `seq` (patMatchesIntStatePdPat0 0 pdStateTable w [(s,b)])
>     allbinders = allbinders' `seq` map snd (filter (\(i,_) -> i `elem` sfinal) allbinders' )
>     -- all_func' = s `seq` pdStateTable `seq` (patMatchesIntStatePdPat0' 0 pdStateTable w [(s,[])])
>     -- all_func = all_func' `seq` map snd (filter (\(i,_) -> i `elem` sfinal) all_func' ) 
>   in map (collectPatMatchFromBinder w) $! allbinders
>      -- map (\fs -> collectPatMatchFromBinder w (applyAll (reverse fs) b)) $! all_func 




> greedyPatMatch :: Pat -> Word -> Maybe Env
> greedyPatMatch p w =
>      first (patMatchIntStatePdPat0 p w)
>   where
>     first (env:_) = return env
>     first _ = Nothing

Compilation


> compilePat :: Pat -> (PdPat0Table, [Int], Binder)
> compilePat p =  (pdStateTable, sfinal, b)
>     where 
>           (pdStateTable,sfinal) = buildPdPat0Table p
>           b = toBinder p

> patMatchIntStateCompiled :: (PdPat0Table, [Int], Binder) -> Word -> [Env]
> patMatchIntStateCompiled (pdStateTable,sfinal,b) w = 
>   let
>     s = 0 
>     -- allbinders' = b `seq` s `seq` pdStateTable `seq` (patMatchesIntStatePdPat0 0 pdStateTable w [(s,b)]) 
>     -- allbinders = allbinders' `seq` map snd (filter (\(i,_) -> i `elem` sfinal) allbinders' )
>     all_func' = s `seq` pdStateTable `seq` (patMatchesIntStatePdPat0' 0 pdStateTable w [(s,[])])
>     all_func = all_func' `seq` map snd (filter (\(i,_) -> i `elem` sfinal) all_func' ) 
>   in -- map (collectPatMatchFromBinder w) allbinders
>      all_func `seq` 
>      map (\fs -> let fs' = reverse fs
>                  in fs' `seq` collectPatMatchFromBinder w (applyAll fs' b)) all_func 

> applyAll :: [ Binder -> Binder ] -> Binder -> Binder
> -- applyAll _  b = b -- fixme
> applyAll [] b = b
> applyAll (f:fs) b = let b' = f b
>                     in b' `seq` applyAll fs b'
              


> greedyPatMatchCompiled :: (PdPat0Table, [Int], Binder) -> Word -> Maybe Env
> greedyPatMatchCompiled compiled w =
>      first (patMatchIntStateCompiled compiled w)
>   where
>     first (env:_) = return env
>     first _ = Nothing





> -- | The PDeriv backend spepcific 'Regex' type

> newtype Regex = Regex (PdPat0Table, [Int], Binder) 


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
>     , newSyntax :: Bool        -- ^ False in blankCompOpt, True in defaultCompOpt. 
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

> digits_re = foldl' (\x y -> Choice x y Greedy) (L '0') (map L "12345789")

> p11 = PPair (PStar (PVar 1 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy) (PPair (PStar (PVar 2 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy) (PPair (PStar (PVar 3 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy) (PStar (PVar 4 [] (PE (Seq digits_re (Star digits_re Greedy)))) Greedy)))

> input11 = S.pack "1234567890123456789-"