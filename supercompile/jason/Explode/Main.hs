module Main where

main :: IO ()
main = print (root (S (S Z)))

{-# SUPERCOMPILE root #-}
root n = case n of
    S n' -> map (\ys -> False : ys) (root n') ++ map (\ys -> True : ys) (root n')
    Z    -> [[]]

data N = S N | Z
