> {- By Kenny Zhuo Ming Lu and Martin Sulzmann, 2009, BSD License -}

A bytestring implementation of reg exp pattern matching using partial derivative
This algorithm exploits the extension of partial derivative of regular expression patterns.
This algorithm implements the POSIX matching policy proceeds by scanning the input word from right to left.

> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
>     FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 


> module Text.Regex.PDeriv.ByteString.Posix
>     ( Regex
>     , CompOption(..)
>     , ExecOption(..)
>     , defaultCompOpt
>     , defaultExecOpt
>     , compile
>     , execute
>     , regexec
>     ) where 


> import System.IO.Unsafe


> import Data.List 
> import Data.Char (ord)
> import GHC.Int
> import GHC.Arr 
> import qualified Data.IntMap as IM
> import qualified Data.ByteString.Char8 as S


> import Text.Regex.Base(RegexOptions(..),RegexLike(..),MatchArray)


> import Text.Regex.PDeriv.RE
> import Text.Regex.PDeriv.Pretty (Pretty(..))
> import Text.Regex.PDeriv.Common (Range, Letter, IsEmpty(..), my_hash, my_lookup, GFlag(..), IsEmpty(..), IsGreedy(..), nub2)
> import Text.Regex.PDeriv.IntPattern (Pat(..), pdPat, toBinder, Binder(..), strip, listifyBinder)
> import Text.Regex.PDeriv.Parse
> import qualified Text.Regex.PDeriv.Dictionary as D (Dictionary(..), Key(..), insertNotOverwrite, lookupAll, empty, isIn, nub)



> logger io = unsafePerformIO io


A word is a byte string.

> type Word = S.ByteString

> rg_collect :: S.ByteString -> (Int,Int) -> S.ByteString
> rg_collect w (i,j) = S.take (j' - i' + 1) (S.drop i' w)
>	       where i' = fromIntegral i
>	             j' = fromIntegral j


----------------------------
-- (greedy) pattern matching

> type Env = [(Int,Word)]


we compile all the possible partial derivative operation into a table
The table maps key to a set of target integer states and their corresponding
binder update functions. 

> type PdPat0TableRev = IM.IntMap [(Int, Int -> Binder -> Binder, Int, Bool)]

A function that builds the above table from the input pattern

> buildPdPat0Table :: Pat -> (PdPat0TableRev, [Int])
> buildPdPat0Table init = 
>     let sig = map (\x -> (x,0)) (sigmaRE (strip init))         --  the sigma
>         init_dict = D.insertNotOverwrite (D.hash init) (init,0) D.empty         --  add init into the initial dictionary
>         (all, delta, dictionary) = sig `seq` builder sig [] [] [init] init_dict 1   --  all states and delta
>         final = all `seq`  [ s | s <- all, isEmpty (strip s)]                   --  the final states
>         sfinal = final `seq` dictionary `seq` map (mapping dictionary) final
>         lists = delta `seq` dictionary `seq` [ (j, l, (i,f,flag,gf)) | (p,l,f,q,flag,gf) <- delta, 
>                                                let i = mapping dictionary p  
>                                                    j = mapping dictionary q
>                                               {- , i `seq` j `seq` True -} ]
>         -- lists_with_pri =  lists `seq` zip lists [0..]
>         hash_table =  {-lists_with_pri `seq`-}
>                      foldl (\ dict (q,x,pf@(p,f,flag,gf)) -> 
>                                  let k = my_hash q (fst x)
>                                  in k `seq` case IM.lookup k dict of 
>                                       Just pfs -> IM.update (\x -> Just (pfs ++ [(p,f,flag,gf)])) k dict
>                                       Nothing -> IM.insert k [(p,f,flag,gf)] dict) IM.empty lists
>     in sfinal `seq` (hash_table, sfinal)



> mapping :: D.Dictionary (Pat,Int) -> Pat -> Int
> mapping dictionary x = let candidates = D.lookupAll (D.hash x) dictionary
>                        in candidates `seq` 
>                           case candidates of
>                             [(_,i)] -> i
>                             _ -> 
>                                 case lookup x candidates of
>                                 (Just i) -> i
>                                 Nothing -> error ("this should not happen. looking up " ++ (pretty x) ++ " from " ++ (show candidates) )

> builder :: [Letter] 
>         -> [Pat] 
>         -> [(Pat,Letter, Int -> Binder -> Binder, Pat, Int, Bool)] 
>         -> [Pat] 
>         -> D.Dictionary (Pat,Int)
>         -> Int 
>         -> ([Pat], [(Pat, Letter, Int -> Binder -> Binder, Pat, Int, Bool)], D.Dictionary (Pat,Int))
> builder sig acc_states acc_delta curr_states dict max_id 
>     | null curr_states  = (acc_states, acc_delta, dict)
>     | otherwise = 
>         let 
>             all_sofar_states = acc_states ++ curr_states
>             new_delta = [ (s, l, f, s', flag, gf ) | s <- curr_states, l <- sig, ((s',f,gf),flag) <- pdPat0Flag s l]
>             new_states = all_sofar_states `seq` D.nub [ s' | (_,_,_,s',_,_) <- new_delta
>                                                       , not (s' `D.isIn` dict) ]
>             acc_delta_next  = (acc_delta ++ new_delta)
>             (dict',max_id') = new_states `seq` foldl (\(d,id) p -> (D.insertNotOverwrite (D.hash p) (p,id) d, id + 1) ) (dict,max_id) new_states
>         in {- dict' `seq` max_id' `seq` -} builder sig all_sofar_states acc_delta_next new_states dict' max_id' 


> pdPat0Flag p l = let qfs = pdPat0 p l
>                  in case qfs of 
>                       []        -> []
>                       [ (q,f,gf) ] -> [ ((q,f,gf),0) ] 
>                       qfs       -> zip qfs [1..]



> -- | Function 'collectPatMatchFromBinder' collects match results from binder 
> collectPatMatchFromBinder :: Word -> Binder -> Env
> collectPatMatchFromBinder w b = collectPatMatchFromBinder_ w (listifyBinder b)


> collectPatMatchFromBinder_ w [] = []
> collectPatMatchFromBinder_ w ((x,[]):xs) = (x,S.empty):(collectPatMatchFromBinder_ w xs)
> collectPatMatchFromBinder_ w ((x,rs):xs) = (x,foldl S.append S.empty $ map (rg_collect w) (id rs)):(collectPatMatchFromBinder_ w xs)

> -- | algorithm right to left scanning single pass
> -- | the "partial derivative" operations among integer states + binders
> lookupPdPat0' :: PdPat0TableRev -> (Int,Binder) -> Letter -> [(Int,Binder,Int,Bool)]
> lookupPdPat0' hash_table (i,b) (l,x) = 
>     case IM.lookup (my_hash i l) hash_table of
>     Just quatripples -> [ (j, op x b, p, gf) | (j, op, p, gf) <- quatripples ]
>     Nothing -> []
 

> patMatchesIntStatePdPat0Rev  :: Int -> PdPat0TableRev -> Word -> [(Int, Binder, Int, Bool)] -> [(Int, Binder, Int, Bool )]
> patMatchesIntStatePdPat0Rev  cnt pdStateTableRev w fs =
>     case S.uncons w of 
>       Nothing -> fs
>       Just (l,w') -> 
>           let 
>               fs' = nubPosix [ (j, b', pri, gf) | (i, b, _, _) <- fs, (j, b', pri, gf) <- lookupPdPat0' pdStateTableRev (i,b) (l,cnt) ]
>               cnt' = cnt - 1
>           in fs' `seq` cnt' `seq` patMatchesIntStatePdPat0Rev cnt' pdStateTableRev w' fs'


> nubPosix :: [(Int,Binder,Int,Bool)] -> [(Int,Binder,Int,Bool)]
> nubPosix [] = []
> nubPosix [x] = [x]                                            -- optimization
> nubPosix ls = 
>     let ls' = nubPosixSub ls
>     in ls'

> nubPosixSub [] = []
> nubPosixSub (x@(k,b,0,_):xs) = 
>     let xs' = nubPosixSub xs
>     in (x:xs')
> nubPosixSub a@(x@(k,b,n,vs):xs) = 
>     let cmp (k1,b1,_,gf1) (k2,b2,_,gf2) = 
>             case compare gf1 gf2 of
>               EQ -> compareBinderLocal b1 b2 --  compare (b1,v1) (b2,v2)
>               ordering -> ordering 
>         ys = [ (k',b',n',gf') | (k',b',n',gf') <- a, k == k' ]
>         zs = [ (k',b',n',gf') | (k',b',n',gf') <- a, not (k == k') ]
>         y = maximumBy cmp ys
>     in if x == y 
>        then y:(nubPosixSub zs)
>        else nubPosixSub xs


> compareBinderLocal :: Binder -> Binder -> Ordering 
> compareBinderLocal bs bs' = 
>     let rs  = map snd (listifyBinder bs)
>         rs' = map snd (listifyBinder bs')
>         os  = map (\ (r,r') -> compareRangeLocal r r')  (zip rs rs')
>     in {- logger (print (show os)) `seq` 
>        logger (print (show bs)) `seq` 
>        logger (print (show bs')) `seq` -}
>        firstNonEQ os

> compareRangeLocal :: [Range] -> [Range] -> Ordering
> compareRangeLocal [] [] = EQ
> compareRangeLocal (x:xs) (y:ys) 
>   | (len x) > (len y) = GT
>   | (len x) == (len y) = compareRangeLocal xs ys
>   | otherwise = LT
> compareRangeLocal (_:_) [] = GT
> compareRangeLocal [] (_:_) = LT
> {-
> compareRangeLocal [] _ = LT
> compareRangeLocal _ [] = GT
> compareRangeLocal (x:xs) (y:ys) 
>   | (len x) < 1 && (len y) < 1 = 
>       compareRangeLocal xs ys
>    -- local, only the most recent is needed
>   | otherwise = 
>       compare (len x) (len y)
> -}

> firstNonEQ :: [Ordering] -> Ordering
> firstNonEQ [] = EQ
> firstNonEQ (EQ:os) = firstNonEQ os
> firstNonEQ (o:_) = o

> len :: Range -> Int
> len (b,e) = e - b + 1


> patMatchIntStatePdPat0Rev :: Pat -> Word -> [Env]
> patMatchIntStatePdPat0Rev p w = 
>     let
>         (pdStateTableRev, fins) = buildPdPat0Table p
>         b = toBinder p
>         l = S.length w
>         w' = S.reverse w
>         fs = [ (i, b, 0, True) | i <- fins ]
>         fs' =  w' `seq` fins `seq` l `seq` pdStateTableRev `seq` (patMatchesIntStatePdPat0Rev (l-1) pdStateTableRev w' fs)
>         -- fs'' = my_sort fs'
>         allbinders = [ b | (s,b,_, _) <- fs', s == 0 ]
>     in map (collectPatMatchFromBinder w) allbinders
>                      

> -- my_sort = sortBy (\ (_,_,ps) (_,_,ps') -> compare ps ps')

 pat = S.pack "^.*foo=([0-9]+).*bar=([0-9]+).*$"


> posixPatMatch :: Pat -> Word -> Maybe Env
> posixPatMatch p w =
>     first ( patMatchIntStatePdPat0Rev p w )
>     where
>     first (env:_) = return env
>     first _ = Nothing


> compilePat :: Pat -> (PdPat0TableRev, [Int], Binder)
> compilePat p = {- pdStateTable `seq` b `seq` -} (pdStateTable, fins, b)
>     where 
>           (pdStateTable,fins) = buildPdPat0Table p
>           b = toBinder p


> patMatchIntStateCompiled ::  (PdPat0TableRev, [Int], Binder) -> Word -> [Env]
> patMatchIntStateCompiled (pdStateTable, fins ,b)  w =
>     let
>         l = S.length w
>         w' = S.reverse w
>         fs = [ (i, b, i, True) | i <- fins ]
>         fs' = w' `seq` fs `seq`  l `seq` pdStateTable `seq` (patMatchesIntStatePdPat0Rev (l-1) pdStateTable w' fs)
>         -- fs'' = fs' `seq` my_sort fs'
>         allbinders = fs' `seq` [  b' | (s,b',_, _) <- fs', s == 0 ]
>     in allbinders `seq` map (collectPatMatchFromBinder w) allbinders
>       

> posixPatMatchCompiled :: (PdPat0TableRev, [Int], Binder) -> Word -> Maybe Env
> posixPatMatchCompiled compiled w =
>      first (patMatchIntStateCompiled compiled w)
>   where
>     first (env:_) = return env
>     first _ = Nothing





a function that updates the binder given an index (that is the pattern var)
ASSUMPTION: the  var index in the pattern is linear. e.g. no ( 0 :: R1, (1 :: R2, 2 :: R3))

> updateBinderByIndex :: Int 
>                     -> Int 
>                     -> Binder 
>                     -> Binder
> updateBinderByIndex i pos binder = 
>     case IM.lookup i binder of
>       { Nothing -> IM.insert i [(pos, pos)] binder
>       ; Just ranges -> 
>         case ranges of 
>         { [] -> IM.update (\_ -> Just [(pos,pos)]) i binder
>         ; ((b,e):rs) 
>           | pos == b - 1  -> IM.update (\_ -> Just ((b-1,e):rs)) i binder
>           | pos < (b - 1) -> IM.update (\_ -> Just ((pos,pos):(b,e):rs)) i binder
>           | otherwise     -> error "impossible, the current letter position is greater than the last recorded letter"
>         }
>       }

> {-
> updateBinderByIndex :: Int    -- ^ pattern variable index
>                        -> Int -- ^ letter position
>                        -> Binder -> Binder
> updateBinderByIndex i lpos binder = 
>     updateBinderByIndexSub lpos i binder 
> 
> -- updateBinderByIndexSub :: Int -> Int -> Binder -> Binder
> updateBinderByIndexSub pos idx [] = []
> updateBinderByIndexSub pos idx  (x@(idx',(b,e):rs):xs)
>     | pos `seq` idx `seq` idx' `seq` xs `seq` False = undefined
>     | idx == idx' && pos == (b - 1) = (idx', (b - 1, e):rs):xs
>     | idx == idx' && pos < (b - 1)  = (idx', (pos,pos):(b, e):rs):xs
>     | idx == idx' && pos > (b - 1)  = error "impossible, the current letter position is greater than the last recorded letter"
>     | otherwise =  x:(updateBinderByIndexSub pos idx xs)
> updateBinderByIndexSub pos idx (x@(idx',[]):xs)
>     | pos `seq` idx `seq` idx' `seq` xs `seq` False = undefined
>     | idx == idx' = ((idx', [(pos, pos)]):xs)
>     | otherwise = x:(updateBinderByIndexSub pos idx xs)
> -}

> resetLocalBnd :: Pat -> Binder -> Binder
> resetLocalBnd p b = 
>   let vs = getVars p
>   in aux vs b 
>      where aux :: [Int] -> Binder -> Binder
>            aux is b = foldl (\b' i -> 
>                              case IM.lookup i b' of
>                                { Nothing -> b'
>                                ; Just [] -> IM.update (\r -> Just r) i b'
>                                ; Just ((s,e):_) -> IM.update (\r -> Just ((s,(s-1)):r)) i b'
>                                }) b is
>                                                       
> {-
>     where aux :: [Int] -> Binder -> Binder
>           aux vs [] = []
>           aux vs ((b@(x,r)):bs) | x `elem` vs = 
>                                     case r of 
>                                       { []        -> (b:(aux vs bs))
>                                       ; ((s,e):_) -> ((x, (s,(s-1)):r):(aux vs bs))
>                                       } 
>                                 | otherwise   =  (b:(aux vs bs))
> -}

retrieve all variables appearing in p

> getVars :: Pat -> [Int] 
> getVars (PVar  x _ p) = x:(getVars p)
> getVars (PPair p1 p2) = (getVars p1) ++ (getVars p2)
> getVars (PPlus p1 p2) = (getVars p1)
> getVars (PStar p1 g)  = (getVars p1)
> getVars (PE r)        = []
> getVars (PChoice p1 p2 _) = (getVars p1) ++ (getVars p2)
> getVars (PEmpty p) = (getVars p)

An specialized version of pdPat0 specially designed for the Posix match
In case of p* we reset in the local binding.

> pdPat0 :: Pat -> Letter -> [(Pat, Int -> Binder -> Binder, Bool )]
> pdPat0 (PVar x w p) (l,idx) 
>     | IM.null (toBinder p) = -- p is not nested
>         let pds = partDeriv (strip p) l
>         in if null pds then []
>            else [ (PVar x [] (PE (resToRE pds)), (\i -> (updateBinderByIndex x i)), True ) ]
>     | otherwise = 
>         let pfs = pdPat0 p (l,idx)
>         in [ (PVar x [] pd, (\i -> (f i) . (updateBinderByIndex x i)  ), True ) | (pd,f, _) <- pfs ]
> pdPat0 (PE r) (l,idx) = 
>     let pds = partDeriv r l
>     in if null pds then []
>        else [ (PE (resToRE pds), ( \_ -> id ), True ) ]
> pdPat0 (PStar p g) l = let pfs = pdPat0 p l
>                            reset  = resetLocalBnd p -- restart all local binder in variables in p
>                        in [ (PPair p' (PStar p g), (\ i -> reset . (f i) ), True) | (p', f, _) <- pfs ]
>                      -- in [ (PPlus p' (PStar p), f) | (p', f) <- pfs ]
> {-
> pdPat0 (PPlus p1 p2@(PStar _)) l  -- we drop this case since it make difference with the PPair
>        | isEmpty (strip p1) = [ (PPlus p3 p2, f) | (p3,f) <- pdPat0 p1 l ] ++ (pdPat0 p2 l) -- simply drop p1 since it is empty
>        | otherwise = [ (PPlus p3 p2, f) | (p3,f) <- pdPat0 p1 l ] 
> -}
> pdPat0 (PPair p1 p2) l = 
>     if (isEmpty (strip p1))
>     then if isGreedy p1
>          then nub3 ([ (PPair p1' p2, f, True) | (p1' , f, _) <- pdPat0 p1 l ] ++ (pdPat0 p2 l))
>          else nub3 ((pdPat0 p2 l) ++ [ (PPair p1' p2, f, False) | (p1' , f, _) <- pdPat0 p1 l ])
>     else [ (PPair p1' p2, f, True) | (p1',f, _) <- pdPat0 p1 l ]
> pdPat0 (PChoice p1 p2 _) l = 
>     nub3 ((pdPat0 p1 l) ++ (pdPat0 p2 l)) -- nub doesn't seem to be essential


> nub3 :: Eq a => [(a,b,c)] -> [(a,b,c)]
> nub3 = nubBy (\(p1,_,_) (p2, _, _) -> p1 == p2) 


> -- | The PDeriv backend spepcific 'Regex' type

> type Regex = (PdPat0TableRev, [Int], Binder) 


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
>       patToRegex p _ _ = (compilePat p)



> execute :: Regex      -- ^ Compiled regular expression
>        -> S.ByteString -- ^ ByteString to match against
>        -> Either String (Maybe Env)
> execute r bs = Right (posixPatMatchCompiled r bs)

> regexec :: Regex      -- ^ Compiled regular expression
>        -> S.ByteString -- ^ ByteString to match against
>        -> Either String (Maybe (S.ByteString, S.ByteString, S.ByteString, [S.ByteString]))
> regexec r bs =
>  case posixPatMatchCompiled r bs of
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

> instance RegexLike Regex S.ByteString where 
> -- matchAll :: regex -> source -> [MatchArray]
>    matchAll = patMatchIntStateCompiledMatchArray
> -- matchOnce :: regex -> source -> Maybe MatchArray
>    matchOnce = posixPatMatchCompiledMatchArray
> -- matchCount :: regex -> source -> Int
> -- matchTest :: regex -> source -> Bool
> -- matchAllText :: regex -> source -> [MatchText source]
> -- matchOnceText :: regex -> source -> Maybe (source, MatchText source, source)
>     



> patMatchIntStateCompiledMatchArray ::  (PdPat0TableRev, [Int], Binder) -> Word -> [MatchArray]
> patMatchIntStateCompiledMatchArray (pdStateTable, fins ,b)  w =
>     let
>         l = S.length w
>         w' = S.reverse w
>         fs = [ (i, b, i, True) | i <- fins ]
>         fs' = w' `seq` fs `seq`  l `seq` pdStateTable `seq` (patMatchesIntStatePdPat0Rev (l-1) pdStateTable w' fs)
>         -- fs'' = fs' `seq` my_sort fs'
>         allbinders = fs' `seq` [  b' | (s,b',_, _) <- fs', s == 0 ]
>         io = logger (print $ show allbinders)
>     in io `seq` allbinders `seq` map (binderToMatchArray l) allbinders

> binderToMatchArray l b  = 
>     let subPatB   = filter (\(x,_) -> x > 0) (listifyBinder b)
>         mbPrefixB = IM.lookup (-1) b
>         mbSubfixB = IM.lookup (-2) b
>         mainB     = case (mbPrefixB, mbSubfixB) of
>                       (Just [(_,x)], Just [(y,_)]) -> (x + 1, y - (x + 1))
>                       (Just [(_,x)], _)            -> (x + 1, l - (x + 1))
>                       (_, Just [(y,_)])            -> (0, y) 
>                       (_, _)                       -> (0, l)
>                       _                            -> error (show (mbPrefixB, mbSubfixB) )
>         rs = map snd subPatB      
>     in listToArray (mainB:(map (\r -> case r of { (_:_) -> fromRange (last r) ; [] -> (-1,0) } ) rs))
>     where fromRange (b,e) = (b, e-b+1)

> listToArray l = listArray (0,length l-1) l

> posixPatMatchCompiledMatchArray :: (PdPat0TableRev, [Int], Binder) -> Word -> Maybe MatchArray
> posixPatMatchCompiledMatchArray compiled w =
>      first (patMatchIntStateCompiledMatchArray compiled w)
>   where
>     first (env:_) = return env
>     first _ = Nothing


> Right r0 = compile defaultCompOpt defaultExecOpt (S.pack "(ab|a)(bc|c)")
> s0 = S.pack "abc"


> Right r1 = compile defaultCompOpt defaultExecOpt (S.pack "^((a|ab)(baa|a))(ac|c)$")
> s1 = S.pack "abaac"


> Right r2 = compile defaultCompOpt defaultExecOpt (S.pack "^((a)|(aa))*$")
> s2 = S.pack "aa"


We should reset after apply f

0: "(xy : ((x : a)|(y: aa)))*"

1: "(xy : (x :(), y:a)) , (xy : ((x : a)|(y: aa)))*"


0, a, 0, _,  [x,xy], !
0, a, 1, _,  [y,xy], !
1, a, 0, D,  [y,xy]


[ (xy,[]), (x,[]), (y,[]) ]

0 <-a 0   [ (xy,[a,!]), (x,[a,!]), (y,[]) ]
  <-a 1   [ (xy,[a]), (x,[]),     (y,[a]) ]

0 <-a 0   [ (xy,[a,!,a,!]), (x,[a,!,a,!]), (y,[]) ]
1 <-a 0   [ (xy,[a,!,a]), (x,[]), (y,[a,!,a]) ]




0, a, 0, _,  [x,xy], !
0, a, 1, _,  [y,xy], !
1, a, 0, D,  [y,xy], 


[ (xy,[]), (x,[]), (y,[]) ]

0 <-a 0   [ (xy,[a,!]), (x,[a,!]), (y,[]) ]
  <-a 1   [ (xy,[a,!]), (x,[]),     (y,[a,!]) ]

0 <-a 0   [ (xy,[a,!,a,!]), (x,[a,!,a,!]), (y,[]) ]
1 <-a 0   [ (xy,[aa,!]), (x,[]), (y,[aa,!]) ]

> Right up5 =  compile defaultCompOpt defaultExecOpt (S.pack "a($)")
> s5 = S.pack "aa"

Searched text: "aa"
Regex pattern: "a($)"
Expected output: "(1,2)(2,2)"
Actual result  : "(1,2)(-1,-1)" 

($) same as () ??
Not match (-1,-1) -> (2,2)


> Right up7 =  compile defaultCompOpt defaultExecOpt (S.pack "(..)*(...)*")
> s7 = S.pack "a"

Searched text: "a"
Regex pattern: "(..)*(...)*"
Expected output: "(0,0)(-1,-1)(-1,-1)"
Actual result  : "(1,1)(-1,-1)(-1,-1)"

because it is an unanchored match, "a" is matched by the -1 sub group?? 
but in the expected output, it should be matched by the -2 sub group??


> Right up8 =  compile defaultCompOpt defaultExecOpt (S.pack "(..)*(...)*")
> s8 = S.pack "abcd"


> Right up64 =  compile defaultCompOpt defaultExecOpt (S.pack "a*")
> s64 = S.pack "aaa"

> Right r64 =  compile defaultCompOpt defaultExecOpt (S.pack "^(a*?)(a*)(a*?)$")

