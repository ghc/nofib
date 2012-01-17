-- | implement generic statistics functions

module Stats (UniVar(..),uniVar
              ,Quantiles(..),quantiles,histogram,display) where

import Data.List (sort,sortBy,group)
import Data.Map hiding (adjust, map, foldl)

class Statistic s where
    samples :: s -> Int

data UniVar = UV { uv_samples  :: Int
                 , mean     :: Double
                 , stdev    :: Double
                 , variance :: Double
                 , skewness :: Double
                 , kurtosis :: Double
                 , sumSq    :: Double
                 , coeffVar :: Double
                 , stdErrMn :: Double
                 }

instance Statistic UniVar where samples = uv_samples

instance Show UniVar where
    show u = adjust
             [["Samples", (show $ samples u)]
             , ["Mean", (show $ mean u)]
             , ["Standard dev", (show $ stdev u)]
             , ["Variance", (show $ variance u)]
             , ["Skewness", (show $ skewness u)]
             , ["Kurtosis", (show $ kurtosis u)]
             , ["Sum of squares", (show $ sumSq u)]
             , ["Coeff. of var", (show $ coeffVar u)]
             , ["Std err mean", (show $ stdErrMn u)]
             ]

adjust :: [[String]] -> String
adjust [] = []
adjust ([a,b]:xs) = (a++":"++take (15-length a) (repeat ' ')++b) ++ "\n" ++ adjust xs

type UVTMP = (Int,Double,Double,Double,Double)

-- | more or less the univariate function from SAS
-- | calculate by tracking n, sum of x, of x², x^3, x^4
uniVar :: [Double] -> UniVar
uniVar ds = let
            (n',x,x2,x3,x4) = foldl uv (0,0,0,0,0) ds -- todo: strict
            n = fromIntegral n'
            m = x/n
            m2 = m*m
            m3 = m*m2
            var  = (x2-m2*n)/(n-1)
            s = sqrt(var)
            skew = (x3 - 3*m*x2 + 2*m3*n)/(s*s*s*n)
            kurt = (x4 - 4*m*x3 + 6*m2*x2 - 4*m3*x + n*m*m3)/(s*s*s*s*n) - 3
            cv = s/m
            in
            UV n' m s var skew kurt x2 cv (s/sqrt n)
    where
    -- helper for uniVar
    uv :: UVTMP -> Double -> UVTMP
    uv (n,x,x2,x3,x4) d = n `seq` x `seq` x2 `seq` x3 `seq` x4 `seq`
                          (n+1,x+d,x2+d*d,x3+d*d*d,x4+d*d*d*d)

data Quantiles = Qs { wsamples   :: Int
                 , smallest  :: Double
                 , quartile1 :: Double
                 , median    :: Double
                 , mode      :: [Double]
                 , quartile3 :: Double
                 , greatest  :: Double
                 }

instance Statistic Quantiles where samples = wsamples

instance Show Quantiles where
    show w = adjust
             [["Samples", (show $ samples w)]
             , ["Smallest", (show $ smallest w)]
             , ["Q1", (show $ quartile1 w)]
             , ["Median", (show $ median w)]
             , ["Modes", (show $ mode w)]
             , ["Q3", (show $ quartile3 w)]
             , ["Greatest", (show $ greatest w)]]

quantiles :: [Double] -> Quantiles
quantiles ds = let
            n = length ds
            sorted = sort ds
            q1 = case n `quotRem` 4 of (q,0) -> ((sorted!!(q-1))+(sorted!!q))/2.0
                                       (q,_) -> sorted!!q
            q2 = case n `quotRem` 2 of (q,0) -> ((sorted!!(q-1))+(sorted!!q))/2.0
                                       (q,_) -> sorted!!q
            q3 = case (3*n) `quotRem` 4 of (q,0) -> ((sorted!!(q-1))+(sorted!!q))/2.0
                                           (q,_) -> sorted!!q
            modes = let
                    ms = sortOn (negate.fst) $ map (\x->(length x,head x))$ group sorted
                    in (snd $ head ms) : map snd
                       (takeWhile (\x->fst x==fst (head ms)) (tail ms))
            in
            Qs n (head sorted) q1 q2 modes q3 (last sorted)

sortOn :: Ord b => (a->b) -> [a] -> [a]
sortOn f = sortBy (\x y -> compare (f x) (f y))

type Histogram = Map Double Int

-- | histogram builds a histogram given the list of midpoints
histogram :: [Double] -> [Double] -> Histogram
histogram ms xs = foldl (insrt ms) emptyFM' xs
    where
    emptyFM' = foldl (\s v -> Data.Map.insert v 0 s) empty ms
    insrt :: [Double] -> Histogram -> Double -> Histogram
    insrt (n1:n2:ns) s x = if abs (n1-x) <= abs (n2-x)
                            then insertWith (+) n1 1 s else insrt (n2:ns) s x
    insrt [n1] s _ = insertWith (+) n1 1 s
    insrt [] _ _ = error "Must provide at least one midpoint"

-- todo: speed up with strict foldl'

display :: Histogram -> String
display h = unlines $ map disp1 $ toList h
    where disp1 (v,n) = show v ++ (take (7-(length $ show v)) (repeat ' ')) ++
                        ": " ++ (take n $ repeat '*')
