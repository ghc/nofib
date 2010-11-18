
> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
>     FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 

> module Text.Regex.PDeriv.RE where

> import Data.List (nub)
> import Data.Char (chr)

> import Text.Regex.PDeriv.Common (IsEmpty(..), IsGreedy(..), GFlag(..))
> import Text.Regex.PDeriv.Dictionary (Key(..), primeL, primeR)

------------------------

> -- | data type of the regular expresions
> data RE = Phi 
>  | Empty        -- ^ an empty exp
>  | L Char	  -- ^ a literal / a character
>  | Choice RE RE GFlag -- ^ a choice exp 'r1 + r2'
>  | Seq RE RE     -- ^ a pair exp '(r1,r2)'
>  | Star RE GFlag -- ^ a kleene's star exp 'r*'
>  | Any           -- ^ .
>  | Not [Char]    -- ^ excluding characters e.g. [^abc]

> -- | the eq instance
> instance Eq RE where
>     (==) Empty Empty = True
>     (==) (L x) (L y) = x == y
>     (==) (Choice r1 r2 g1) (Choice r3 r4 g2) = (g1 == g2) && (r2 == r4) && (r1 == r3) 
>     (==) (Seq r1 r2) (Seq r3 r4) = (r1 == r3) && (r2 == r4)
>     (==) (Star r1 g1) (Star r2 g2) = g1 == g2 && r1 == r2 
>     (==) Any Any = True
>     (==) (Not cs) (Not cs') = cs == cs'
>     (==) _ _ = False


> -- | A pretty printing function for regular expression
> instance Show RE where
>     show Phi = "{}"
>     show Empty = "<>"
>     show (L c) = show c
>     show (Choice r1 r2 g) = "(" ++ show r1 ++ "|" ++ show r2 ++ ")" ++ show g
>     show (Seq r1 r2) = "<" ++ show r1 ++ "," ++ show r2 ++ ">"
>     show (Star r g) = show r ++ "*" ++ show g
>     show Any = "."
>     show (Not cs) = "[^" ++ cs ++ "]"

> instance IsGreedy RE where
>     isGreedy Phi = True
>     isGreedy Empty = False
>     isGreedy (Choice r1 r2 Greedy) = True
>     isGreedy (Choice r1 r2 NotGreedy) = False -- (isGreedy r1) || (isGreedy r2)
>     isGreedy (Seq r1 r2) = (isGreedy r1) || (isGreedy r2)
>     isGreedy (Star r Greedy) = True
>     isGreedy (Star r NotGreedy) = False
>     isGreedy (L _) = True
>     isGreedy Any = True
>     isGreedy (Not _) = True

> instance Key RE where
>     hash Phi = [0]
>     hash Empty = [1]
>     hash (Choice r1 r2 Greedy) = {- let x1 = head (hash r1)
>                                      x2 = head (hash r2)
>                                  in [ 3 +  x1 * primeL + x2 * primeR ] -} [3]
>     hash (Choice r1 r2 NotGreedy) = {- let x1 = head (hash r1)
>                                         x2 = head (hash r2)
>                                     in [ 4 + x1 * primeL + x2 * primeR ] -} [4]
>     hash (Seq r1 r2) = {- let x1 = head (hash r1)
>                            x2 = head (hash r2)
>                        in [ 5 + x1 * primeL + x2 * primeR ] -} [5]
>     hash (Star r Greedy) = {- let x = head (hash r)
>                            in [ 6 + x * primeL ] -} [6]
>     hash (Star r NotGreedy) = {- let x = head (hash r)
>                             in [ 7 + x * primeL ] -} [7]
>     hash (L c) = {- let x = head (hash c)
>                  in [ 8 + x * primeL ] -} [8]
>     hash Any = [2]
>     hash (Not _) = [9]



> -- | function 'resToRE' sums up a list of regular expressions with the choice operation.
> resToRE :: [RE] -> RE
> resToRE (r:res) = foldl (\x y -> Choice x y Greedy) r res
> resToRE [] = Phi

> -- | function 'isEmpty' checks whether regular expressions are empty
> instance IsEmpty RE where
>   isEmpty Phi = False
>   isEmpty Empty = True
>   isEmpty (Choice r1 r2 g) = (isEmpty r1) || (isEmpty r2)
>   isEmpty (Seq r1 r2) = (isEmpty r1) && (isEmpty r2)
>   isEmpty (Star r g) = True
>   isEmpty (L _) = False
>   isEmpty Any = False
>   isEmpty (Not _) = False
        

> -- | function 'partDeriv' implements the partial derivative operations for regular expressions. We don't pay attention to the greediness flag here.
> partDeriv :: RE -> Char -> [RE]
> partDeriv r l = nub (partDerivSub r l)


> partDerivSub Phi l = []
> partDerivSub Empty l = []
> partDerivSub (L l') l 
>     | l == l'   = [Empty]
>     | otherwise = []
> partDerivSub Any l = [Empty]
> partDerivSub (Not cs) l 
>     | l `elem` cs = []
>     | otherwise = [Empty]
> partDerivSub (Choice r1 r2 g) l = 
>     let 
>         s1 = partDerivSub r1 l 
>         s2 = partDerivSub r2 l
>     in {- s1 `seq` s2 `seq` -} (s1 ++ s2)
> partDerivSub (Seq r1 r2) l 
>     | isEmpty r1 = 
>           let 
>               s0 = partDerivSub r1 l
>               s1 = s0 `seq` [ (Seq r1' r2) | r1' <- s0 ]
>               s2 = partDerivSub r2 l
>           in {- s1 `seq` s2 `seq` -} (s1 ++ s2)
>     | otherwise = 
>         let 
>             s0 = partDerivSub r1 l 
>         in {- s0 `seq` -} [ (Seq r1' r2) | r1' <- s0 ]
> partDerivSub (Star r g) l = 
>     let
>         s0 = partDerivSub r l
>     in {- s0 `seq` -} [ (Seq r' (Star r g)) | r' <- s0 ]

> -- | function 'sigmaRE' returns all characters appearing in a reg exp.
> sigmaRE :: RE -> [Char]
> sigmaRE r = let s = (sigmaREsub r)
>             in s `seq` nub s

> sigmaREsub (L l) = [l]
> sigmaREsub Any = map chr [32 .. 127]
> sigmaREsub (Not cs) = filter (\c -> not (c `elem` cs)) (map chr [32 .. 127])
> sigmaREsub (Seq r1 r2) = (sigmaREsub r1) ++ (sigmaREsub r2) 
> sigmaREsub (Choice r1 r2 g) = (sigmaREsub r1) ++ (sigmaREsub r2) 
> sigmaREsub (Star r g) = sigmaREsub r
> sigmaREsub Phi = []
> sigmaREsub Empty = []

