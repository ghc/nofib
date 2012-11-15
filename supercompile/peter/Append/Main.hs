module Main where

import System.Environment

main :: IO ()
main = do
    [n_str] <- getArgs
    let n = read n_str
    print (length (root (my_replicate n 'x') (my_replicate n 'y') (my_replicate n 'z')))

-- Prevent benchmark results being skewed because rewrite rules have access to more information than the SC does:
my_replicate :: Int -> a -> [a]
my_replicate 0 x = []
my_replicate n x = x : my_replicate (n - 1) x

{-# SUPERCOMPILE root #-}
root xs ys zs = (xs ++ ys) ++ zs
