module Main where

main :: IO ()
main = print (root (S Z))

double y r = case y of Z   -> r
                       S x -> double x (S (S r))

even y = case y of Z -> True
                   S z -> case z of Z   -> False
                                    S x -> even x

{-# SUPERCOMPILE root #-}
root x = even (double x Z)

data Nat = Z | S Nat
