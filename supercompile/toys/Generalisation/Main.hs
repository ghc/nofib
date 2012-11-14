module Main where

import System.Environment

import Prelude hiding (length)

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (fromPeano 0 (length [1..n]))

fromPeano :: Int -> Nat -> Int
fromPeano n Z     = n
fromPeano n (S x) = n `seq` fromPeano (n + 1) x

foldl' c n xs = case xs of [] -> n; (x:xs) -> let n' = c n x in n' `seq` foldl' c n' xs
{-# SUPERCOMPILE length #-}
length xs = foldl' (\len _ -> S len) Z xs

data Nat = S Nat | Z
         deriving (Eq, Show)
