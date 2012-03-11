module Main where

import Prelude hiding (foldr, map, (.))

main :: IO ()
main = print (root (+1) (+1) [1, 2, 3])

foldr c n xs = case xs of [] -> n; (y:ys) -> c y (foldr c n ys)
map f = foldr (\x xs -> f x : xs) []
(.) f g x = f (g x)

{-# SUPERCOMPILE root #-}
root f g xs = (map (f . g) xs, (map f . map g) xs)
