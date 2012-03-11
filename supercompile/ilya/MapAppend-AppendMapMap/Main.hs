module Main where

import Prelude hiding (foldr, (++), map)

main :: IO ()
main = print (root (+1) [1, 2] [3, 4])

foldr c n xs = case xs of [] -> n; (y:ys) -> c y (foldr c n ys)
(++) xs ys = foldr (:) ys xs
map f = foldr (\x xs -> f x : xs) []

{-# SUPERCOMPILE root #-}
root f xs ys = (map f (xs ++ ys), map f xs ++ map f ys)
