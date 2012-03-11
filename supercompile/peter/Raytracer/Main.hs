module Main where

import System.Enivronment

main :: IO ()
main = do
    n <- fmap (map read) getArgs
    print (root (replicate n 1) (replicate n 2))

{-# SUPERCOMPILE root #-}
root :: [Int] -> [Int] -> Int
root xs ys = sum (zipWith (*) xs ys)
