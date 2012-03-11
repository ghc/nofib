module Main where

import Prelude hiding (map, filter, foldr, (++), (.))

main :: IO ()
main = print (root (<= 2) (+1) [1, 2, 3, 4])

foldr c n xs = case xs of [] -> n; (y:ys) -> c y (foldr c n ys)
(++) xs ys = foldr (:) ys xs
map f = foldr (\x xs -> f x : xs) []
filter p = foldr (\x xs -> if p x then x : xs else xs) []
(.) f g x = f (g x)

{-# SUPERCOMPILE root #-}
root p f xs = (filter p (map f xs), map f (filter (p . f) xs))
