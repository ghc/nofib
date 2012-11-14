module Main where

import System.Environment

import Prelude hiding (reverse)

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (length (root [1..n]))

reverse xs = reverseacc [] xs
  where reverseacc ys xs = case xs of [] -> ys; (x:xs) -> reverseacc (x:ys) xs

{-# SUPERCOMPILE root #-}
root = \xs -> reverse (reverse xs)
