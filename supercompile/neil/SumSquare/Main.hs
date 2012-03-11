module Main where

main :: IO ()
main = print (f 10000)

{-# SUPERCOMPILE f #-}
f :: Int -> Int
f n = sum'Int [ k * m | k <- enumFromTo'Int 1 n, m <- enumFromTo'Int 1 k ]
