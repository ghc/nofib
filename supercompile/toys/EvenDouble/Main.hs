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

double y r = case y of Z   -> r
                       S x -> double x (S (S r))

even y = case y of Z -> True
                   S z -> case z of Z   -> False
                                    S x -> even x

{-# SUPERCOMPILE root #-}
root x = even (double x Z)

data Nat = Z | S Nat
