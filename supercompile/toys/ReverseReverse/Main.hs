module Main where

import Prelude hiding (reverse)

main :: IO ()
main = print (root [1, 2, 3])

reverse xs = reverseacc [] xs
  where reverseacc ys xs = case xs of [] -> ys; (x:xs) -> reverseacc (x:ys) xs

{-# SUPERCOMPILE root #-}
root = \xs -> reverse (reverse xs)
