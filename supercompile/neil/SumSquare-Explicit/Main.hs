module Main where

import Data.List (foldl')

main :: IO ()
main = print (f 10000)

strictSum = foldl' (+) 0

-- Result at Integer type: 1250416704167500
{-# SUPERCOMPILE f #-}
f :: Int -> Int
f n = strictSum go0
  where
    -- TQ translation of [ k * m | k <- enumFromTo 1 n, m <- enumFromTo 1 k ]
    go0 = go1 [] (enumFromTo 1 n)

    go1 tl []     = tl
    go1 tl (k:ks) = go2 (go1 tl ks) (enumFromTo 1 k)
      where
        go2 tl []     = tl
        go2 tl (m:ms) = (k*m) : go2 tl ms
