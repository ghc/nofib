module Main where

import Prelude hiding (even)

main :: IO ()
main = print (root (S Z))

double y = case y of Z   -> Z
                     S x -> S (S (double x))

even y = case y of Z -> True
                   S z -> case z of Z   -> False
                                    S x -> even x

{-# SUPERCOMPILE root #-}
root x = even (double x)

data Nat = Z | S Nat
