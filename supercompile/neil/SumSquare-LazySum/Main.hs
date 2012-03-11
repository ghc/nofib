module Main where

main :: IO ()
main = print (f 10)

lazySum :: [Int] -> Int
lazySum = foldl (+) 0

{-# SUPERCOMPILE f #-}
f :: Int -> Int
f n = lazySum [ k * m | k <- enumFromTo'Int 1 n, m <- enumFromTo'Int 1 k ]
