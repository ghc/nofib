module Main where

import System.Environment

import Prelude hiding (sum)

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (root (replicate n 1) (replicate n 2))

{-# SUPERCOMPILE root #-}
root :: [Int] -> [Int] -> Int
root xs ys = sum (zipWith (*) xs ys)

-- Had to copy this out of the libraries because the default sum is a lazy sum:
-- NB: there are no rewrite rules from sum to foldr in the base libraries (probably because the Report version is defined with foldl)
sum :: Num a => [a] -> a
sum     l       = sum' l 0
  where
    sum' []     a = a
    sum' (x:xs) a = a `seq` sum' xs (a+x)