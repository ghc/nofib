module Main where

import Data.List (foldl')

main :: IO ()
main = print (f 10000)

strictSum = foldl' (+) 0

-- Result at Integer type: 1250416704167500
{-# SUPERCOMPILE f #-}
f :: Int -> Int
f n = strictSum [ k * m | k <- enumFromTo 1 n, m <- enumFromTo 1 k ]
