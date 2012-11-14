module Main where

import System.Environment

import Prelude hiding (even)

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (root (toPeano n))

toPeano :: Int -> Nat
toPeano 0 = Z
toPeano n = S (toPeano (n - 1))

double y = case y of Z   -> Z
                     S x -> S (S (double x))

even y = case y of Z -> True
                   S z -> case z of Z   -> False
                                    S x -> even x

{-# SUPERCOMPILE root #-}
root x = even (double x)

data Nat = Z | S Nat
