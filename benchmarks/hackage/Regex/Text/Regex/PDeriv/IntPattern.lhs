> -- | This module defines the data type of internal regular expression pattern, 
> -- | as well as the partial derivative operations for regular expression patterns.
> module Text.Regex.PDeriv.IntPattern 
>     ( Pat(..)
>     , strip
>     , pdPat
>     , Binder
>     , toBinder
>     , listifyBinder
>  --  , updateBinderByIndex
>     , pdPat0
>     , nub2
>     )
>     where

> import Data.List
> import qualified Data.IntMap as IM
> import Text.Regex.PDeriv.Common (Range, Letter, IsEmpty(..), GFlag(..), IsGreedy(..) )
> import Text.Regex.PDeriv.RE
> import Text.Regex.PDeriv.Dictionary (Key(..), primeL, primeR)
> import Text.Regex.PDeriv.Pretty


> -- | regular expression patterns
> data Pat = PVar Int [Range] Pat       -- ^ variable pattern 
>   | PE RE                             -- ^ pattern without binder
>   | PPair Pat Pat                     -- ^ pair pattern
>   | PChoice Pat Pat GFlag             -- ^ choice pattern 
>   | PStar Pat GFlag                   -- ^ star pattern 
>   | PPlus Pat Pat                     -- ^ plus pattern, it is used internally to indicate that it is unrolled from a PStar
>   | PEmpty Pat                        -- ^ empty pattern, it is used intermally to indicate that mkEmpty function has been applied.


> {-| The Eq instance for Pat data type
>     NOTE: We ignore the 'consumed word' when comparing patterns
>     (ie we only compare the pattern structure).
>     Essential for later comparisons among patterns. -}

> instance Eq Pat where
>   (==) (PVar x1 _ p1) (PVar x2 _ p2) = (x1 == x2) && (p1 == p2) 
>   (==) (PPair p1 p2) (PPair p1' p2') = (p1 == p1') && (p2 == p2')
>   (==) (PChoice p1 p2 g1) (PChoice p1' p2' g2) = (g1 == g2) && (p2 == p2') && (p1 == p1') -- more efficient, because choices are constructed in left-nested
>   (==) (PPlus p1 p2) (PPlus p1' p2') = (p1 == p1') && (p2 == p2')
>   (==) (PStar p1 g1) (PStar p2 g2) =  (g1 == g2) && (p1 == p2)
>   (==) (PE r1) (PE r2) = r1 == r2
>   (==) _ _ = False
> 

> instance Pretty Pat where
>     pretty (PVar x1 _ p1) = "(" ++ show x1 ++ ":" ++ pretty p1 ++ ")"
>     pretty (PPair p1 p2) = "<" ++ pretty p1 ++ "," ++ pretty p2 ++ ">"
>     pretty (PChoice p1 p2 g) = "(" ++ pretty p1 ++ "|" ++ pretty p2 ++ ")" ++ (show g)
>     pretty (PE r) = show r
>     pretty (PPlus p1 p2 ) = "(" ++ pretty p1 ++ "," ++ pretty p2 ++ ")"
>     pretty (PStar p g) = (pretty p) ++ "*" ++ (show g)
>     pretty (PEmpty p) = "[" ++ pretty p ++ "]"

> instance Show Pat where
>     show pat = pretty pat

> instance Key Pat where
>     hash (PVar x1 _ p1) = let y1 = head (hash x1) 
>                               y2 = head (hash p1)
>                           in y1 `seq` y2 `seq` [ 1 + y1 * primeL + y2 * primeR ] 
>     hash (PPair p1 p2) = let x1 = head (hash p1)
>                              x2 = head (hash p2)
>                          in x1 `seq` x2 `seq` [ 2 + x1 * primeL + x2 * primeR ] 
>     hash (PChoice p1 p2 Greedy) = let x1 = head (hash p1)
>                                       x2 = head (hash p2)
>                                   in x1 `seq` x2 `seq`  [ 4 + x1 * primeL + x2 * primeR ] 
>     hash (PChoice p1 p2 NotGreedy) = let x1 = head (hash p1)
>                                          x2 = head (hash p2)
>                                      in x1 `seq` x2 `seq` [ 5 + x1 * primeL + x2 * primeR ]
>     hash (PPlus p1 p2) = let x1 = head (hash p1)
>                              x2 = head (hash p2)
>                          in x1 `seq` x2 `seq` [ 6 + x1 * primeL + x2 * primeR ]
>     hash (PStar p Greedy) = let x = head (hash p)
>                             in x `seq` [ 7 + x * primeL ]
>     hash (PStar p NotGreedy) = let x = head (hash p)
>                             in x `seq` [ 8 + x * primeL ]
>     hash (PE r) = let x = head (hash r)
>                   in x `seq` [ 9 + x * primeL ]
>     hash (PEmpty p) = let x = head (hash p)
>                       in x `seq` [ 3 + x * primeL ]
>

> -- | function 'strip' strips away the bindings from a pattern
> strip :: Pat -> RE 
> strip (PVar _ w p) = strip p
> strip (PE r) = r
> strip (PStar p g) = Star (strip p) g
> strip (PPair p1 p2) = Seq (strip p1) (strip p2)
> strip (PPlus p1 p2) = Seq (strip p1) (strip p2)
> strip (PChoice p1 p2 g) = Choice (strip p1) (strip p2) g
> strip (PEmpty p) = strip p


> -- | function 'mkEmpPat' makes an empty pattern
> mkEmpPat :: Pat -> Pat
> mkEmpPat (PVar x w p) = PVar x w (mkEmpPat p)
> mkEmpPat (PE r) 
>   | isEmpty r = PE Empty
>   | otherwise = PE Phi
> mkEmpPat (PStar p g) = PE Empty -- problematic?! we are losing binding (x,()) from  ( x : a*) ~> PE <>
> mkEmpPat (PPlus p1 p2) = mkEmpPat p1 -- since p2 must be pstar we drop it. If we mkEmpPat p2, we need to deal with pdPat (PPlus (x :<>) (PE <>)) l
> mkEmpPat (PPair p1 p2) = PPair (mkEmpPat p1) (mkEmpPat p2)
> mkEmpPat (PChoice p1 p2 g) = PChoice (mkEmpPat p1) (mkEmpPat p2) g

> {-| function 'pdPat' computes the partial derivatives of a pattern w.r.t. a letter.
>    Integrating non-greedy operator with PStar
>    For p*, we need to unroll it into a special construct
>    say PPlus p' p* where p' \in p/l.
>    When we push another label, say l' to PPlus p' p*, and
>    p' is emptiable, naively, we would do 
>    [ PPlus p'' p* | p'' <- p' / l ] ++ [ PPlus (mkE p') (PPlus p''' p*) | (PPlus p''' p*) <- p*/l ]
>    Now the problem here is the shape of the pdpat are infinite, which 
>    breaks the requirement of getting a compilation scheme.
>    The fix here is to simplify the second component, by combining the binding, of (mkE p') and p'''
>    since they share the same set of variables.
>    [ PPlus p'' p* | p'' <- p' / l ] ++ [ PPlus p4 p* | (PPlus p''' p*) <- p*/l ] 
>    where p4 = combineBinding (mkE p') p'''
>    For pdPat0 approach, we do not need to do this explicitly, we simply drop 
>    (mkE p') even in the PPair case. see the definitely of pdPat0 below
> -}
> pdPat :: Pat -> Letter -> [Pat]
> pdPat (PVar x w p) (l,idx) = 
>          let pds = pdPat p (l,idx)
>          in if null pds then []
>             else case w of
>		  [] -> [ PVar x [ (idx,idx) ] pd | pd <- pds ]
>		  ((b,e):rs)      --  ranges are stored in the reversed manner, the first pair the right most segment
>                     | idx == (e + 1) -> -- it is consecutive
>                         [ PVar x ((b,idx):rs) pd | pd <- pds ]
>                     | otherwise ->      -- it is NOT consecutive
>                         [ PVar x ((idx,idx):(b,e):rs) pd | pd <- pds ]
> pdPat (PE r) (l,idx) = let pds = partDeriv r l 
>                  in if null pds then []
>                     else [ PE $ resToRE pds ]
> {-| The non-greedy operator has impact to a sequence pattern if the 
>     first sub pattern is non-greedy. We simply swap the order of the 
>     'choices' in the resulting pds. -} 
> pdPat (PPair p1 p2) l = 
>   if (isEmpty (strip p1))
>   then  if isGreedy p1
>         then nub ([ PPair p1' p2 | p1' <- pdPat p1 l] ++ 
>                   [ PPair (mkEmpPat p1) p2' | p2' <- pdPat p2 l])
>         else nub ( [ PPair (mkEmpPat p1) p2' | p2' <- pdPat p2 l] ++ [ PPair p1' p2 | p1' <- pdPat p1 l] )
>   else [ PPair p1' p2 | p1' <- pdPat p1 l ]
> {-| Integrating non-greedy operator with pstar requires more cares.
>     We have two questions to consider.
>      1) When we unfold p*, do we need to take the non-greediness in p into consideration?
>         The answer is no. 
>         What happens is that when we unfold p* into p and p*, if we were considering p is non-greedy and apply pdpat to p*
>         again, i.e. mkE(p),p*/l we will run into non-terminating problem. This seems to be bug with python re library. -} 
> pdPat (PStar p g) l = let pds = pdPat p l
>                       in [ PPlus pd (PStar p g) | pd <- pds ]
> {-| 2) After we unfold p* and 'advance' to (PPlus p' p*) say p' \in p / l for some l
>        we are in the position of 'pushing' another label l' into  (PPlus p' p*). 
>     Shall we swap the order of the alternatives when p' is non-greedy?
>     Why not? This seems harmless since we have already made some progress by pushing l into p*. -}
> pdPat (PPlus p1 p2@(PStar _ _)) l -- p2 must be pStar
>     | isEmpty (strip p1) = 
>         if isGreedy p1 
>         then [ PPlus p3 p2 | p3  <- pdPat p1 l ] ++ [ PPlus p3 p2' | (PPlus p1' p2') <- pdPat p2 l, let p3 =  p1' `getBindingsFrom` p1 ]
>         else [ PPlus p3 p2' | (PPlus p1' p2') <- pdPat p2 l, let p3 =  p1' `getBindingsFrom` p1 ] ++ [ PPlus p3 p2 | p3  <- pdPat p1 l ]
>     | otherwise          = [ PPlus p3 p2 | p3  <- pdPat p1 l ]
> pdPat (PChoice p1 p2 g) l = 
>    nub ((pdPat p1 l)  ++ (pdPat p2 l)) -- nub doesn't seem to be essential
> pdPat p l = error ((show p) ++ (show l))

> -- | function 'getBindingsFrom' transfer bindings from p2 to p1
> getBindingsFrom :: Pat  -- ^ the source of the  
>                    -> Pat -> Pat
> getBindingsFrom p1 p2 = let b = toBinder p2
>                         in assign p1 b
>     where assign :: Pat -> Binder -> Pat
>           assign (PVar x w p) b = case IM.lookup x b of
>                                     Nothing -> let p' = assign p b in PVar x w p'
>                                     Just rs -> let p' = assign p b in PVar x (w ++ rs) p'
>           assign (PE r) _ = PE r
>           assign (PPlus p1 p2) b = PPlus (assign p1 b) p2 -- we don't need to care about p2 since it is a p*
>           assign (PPair p1 p2) b = PPair (assign p1 b) (assign p2 b)
>           assign (PChoice p1 p2 g) b = PChoice (assign p1 b) (assign p2 b) g



> -- | Function 'isGreedy' checks whether a pattern is greedy
> instance IsGreedy Pat where
>     isGreedy (PVar _ _ p) = isGreedy p
>     isGreedy (PE r) = isGreedy r
>     isGreedy (PPair p1 p2) = isGreedy p1 || isGreedy p2
>     isGreedy (PChoice p1 p2 Greedy) = True
>     isGreedy (PChoice p1 p2 NotGreedy) = False -- isGreedy p1 || isGreedy p2
>     isGreedy (PEmpty p) = False
>     isGreedy (PStar p Greedy) = True
>     isGreedy (PStar p NotGreedy) = False
>     isGreedy (PPlus p p') = isGreedy p || isGreedy p'


> -- | The 'Binder' type denotes a set of (pattern var * range) pairs
> -- type Binder = [(Int, [Range])]
> type Binder = IM.IntMap [Range]


> -- | Function 'toBinder' turns a pattern into a binder
> toBinder :: Pat -> Binder
> toBinder p = IM.fromList (toBinderList p)

> toBinderList :: Pat -> [(Int, [Range])]
> toBinderList  (PVar i rs p) = [(i,rs)] ++ (toBinderList p)
> toBinderList  (PPair p1 p2) = (toBinderList p1) ++ (toBinderList p2)
> toBinderList  (PPlus p1 p2) = (toBinderList p1) 
> toBinderList  (PStar p1 g)    = (toBinderList p1) 
> toBinderList  (PE r)        = []
> toBinderList  (PChoice p1 p2 g) = (toBinderList p1) ++ (toBinderList p2)
> toBinderList  (PEmpty p) = toBinderList p

> listifyBinder :: Binder -> [(Int, [Range])]
> listifyBinder b = sortBy (\ x y -> compare (fst x) (fst y)) (IM.toList b)
>                   

> {-| Function 'updateBinderByIndex' updates a binder given an index to a pattern var
>     ASSUMPTION: the var index in the pattern is linear. e.g. no ( 0 :: R1, (1 :: R2, 2 :a: R3))
> -}

> updateBinderByIndex :: Int 
>                     -> Int 
>                     -> Binder 
>                     -> Binder
> updateBinderByIndex i pos binder = -- binder  
>     IM.update (\ r -> case r of  -- we always initialize to [], we don't need to handle the key miss case
>                       { [] -> Just [(pos,pos)]
>                       ; ((b,e):rs)
>                           | pos == e + 1 -> Just ((b,e+1):rs)
>                           | pos > e + 1  -> Just ((pos,pos):(b,e):rs)
>                           | otherwise    -> error "impossible, the current letter position is smaller than the last recorded letter"   
>                       } ) i binder 
> {-
> updateBinderByIndex i pos binder = 
>     case IM.lookup i binder of
>       { Nothing -> IM.insert i [(pos, pos)] binder
>       ; Just ranges -> 
>         case ranges of 
>         { [] -> IM.update (\_ -> Just [(pos,pos)]) i binder
>          ; ((b,e):rs)
>           | pos == e + 1  -> IM.update (\_ -> Just ((b,e+1):rs)) i binder 
>           | pos > e + 1 -> IM.update (\_ -> Just ((pos,pos):(b,e):rs)) i binder
>           | otherwise     -> error "impossible, the current letter position is smaller than the last recorded letter"   
>         }
>       }
> -}
> {-
> {-# INLINE updateBinderByIndex #-}
> updateBinderByIndex :: Int    -- ^ the indext of the pattern variable
>                        -> Int -- ^ the letter position
>                        -> Binder -> Binder
> updateBinderByIndex i lpos binder =
>     updateBinderByIndexSub i lpos binder 
> 
> {-# INLINE updateBinderByIndexSub #-}
> updateBinderByIndexSub :: Int -> Int -> Binder -> Binder
> updateBinderByIndexSub idx pos [] = []
> updateBinderByIndexSub idx pos (x@(idx',(b,e):rs):xs)
>     -- | pos `seq` idx `seq` idx' `seq` xs `seq` False = undefined
>     | idx == idx' = if pos == (e + 1)
>                     then (idx', (b, e+ 1):rs):xs
>                     else if pos > (e + 1) 
>                          then (idx', (pos,pos):(b, e):rs):xs
>                          else error "impossible, the current letter position is smaller than the last recorded letter"
>     | otherwise = -- idx `seq` pos `seq` xs `seq` 
>                    x:(updateBinderByIndexSub idx pos xs)
> updateBinderByIndexSub idx pos (x@(idx',[]):xs)
>     -- | pos `seq` idx `seq` idx' `seq` xs `seq` False = undefined
>     | idx == idx' = ((idx', [(pos, pos)]):xs)
>     | otherwise = -- idx `seq` pos `seq` xs `seq`  
>                   x:(updateBinderByIndexSub idx pos xs)
> -} 

> {-| Function 'pdPat0' is the 'abstracted' form of the 'pdPat' function
>     It computes a set of pairs. Each pair consists a 'shape' of the partial derivative, and
>     an update function which defines the change of the pattern bindings from the 'source' pattern to 
>     the resulting partial derivative. This is used in the compilation of the regular expression pattern -}
> pdPat0 :: Pat  -- ^ the source pattern
>           -> Letter -- ^ the letter to be "consumed"
>           -> [(Pat, Int -> Binder -> Binder)]
> pdPat0 (PVar x w p) (l,idx) 
>     | IM.null (toBinder p) = -- p is not nested
>         let pds = partDeriv (strip p) l
>         in g `seq` pds `seq` if null pds then []
>                              else [ (PVar x [] (PE (resToRE pds)), g) ]
>     | otherwise = 
>         let pfs = pdPat0 p (l,idx)
>         in g `seq` pfs `seq` [ (PVar x [] pd, (\i -> (g i) . (f i) )) | (pd,f) <- pfs ]
>     where g = updateBinderByIndex x
> pdPat0 (PE r) (l,idx) = 
>     let pds = partDeriv r l
>     in  pds `seq` if null pds then []
>                   else [ (PE (resToRE pds), ( \_ -> id ) ) ]
> pdPat0 (PStar p g) l = let pfs = pdPat0 p l
>                        in pfs `seq` [ (PPair p' (PStar p g), f) | (p', f) <- pfs ]
> pdPat0 (PPair p1 p2) l = 
>     if (isEmpty (strip p1))
>     then if isGreedy p1
>          then nub2 ([ (PPair p1' p2, f) | (p1' , f) <- pdPat0 p1 l ] ++ (pdPat0 p2 l))
>          else nub2 ((pdPat0 p2 l) ++ [ (PPair p1' p2, f) | (p1' , f) <- pdPat0 p1 l ])
>     else [ (PPair p1' p2, f) | (p1',f) <- pdPat0 p1 l ]
> pdPat0 (PChoice p1 p2 g) l = 
>      nub2 ((pdPat0 p1 l) ++ (pdPat0 p2 l)) -- nub doesn't seem to be essential


> nub2 :: Eq a => [(a,b)] -> [(a,b)]
> nub2 = nubBy (\(p1,f1) (p2, f2) -> p1 == p2) 
