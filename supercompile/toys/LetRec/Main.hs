module Main where

import System.Environment

import Prelude hiding (sum)

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (root n)

{-# SUPERCOMPILE root #-}
root :: Int -> Int
root n = sum (take n example1) + sum (take n example2)
  where
    example1 = let ones = 1 : ones
               in map (\x -> x + 1) ones
    example2 = map (\x -> x + 1) (repeat 1)

-- Had to copy this out of the libraries because the default sum is a lazy sum:
-- NB: there are no rewrite rules from sum to foldr in the base libraries (probably because the Report version is defined with foldl)
sum :: Num a => [a] -> a
sum     l       = sum' l 0
  where
    sum' []     a = a
    sum' (x:xs) a = a `seq` sum' xs (a+x)