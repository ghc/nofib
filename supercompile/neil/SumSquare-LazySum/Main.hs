module Main where

main :: IO ()
main = print (f 10)

lazySum = foldl (+) 0

{-# SUPERCOMPILE f #-}
f :: Int -> Int
f n = lazySum [ k * m | k <- enumFromTo 1 n, m <- enumFromTo 1 k ]
