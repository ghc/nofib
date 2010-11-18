module SumSqPrim (sumSq)
where

import Data.Array.Parallel.Unlifted as U

sumSq :: Int -> Int
{-# NOINLINE sumSq #-}
sumSq n = U.sum (U.map (\x -> x * x) (U.enumFromTo 1 n))
