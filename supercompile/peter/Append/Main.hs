module Main where

import System.Environment

main :: IO ()
main = do
    [n_str] <- getArgs
    let n = read n_str
    print (length (root (replicate n 'x') (replicate n 'y') (replicate n 'z')))

{-# SUPERCOMPILE root #-}
root xs ys zs = (xs ++ ys) ++ zs
