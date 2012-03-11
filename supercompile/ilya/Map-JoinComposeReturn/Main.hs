module Main where

import Prelude hiding (foldr, (++), map, (.), return, join)

main :: IO ()
main = print (root (+1) [1, 2, 3])

foldr c n xs = case xs of [] -> n; (y:ys) -> c y (foldr c n ys)
(++) xs ys = foldr (:) ys xs
map f = foldr (\x xs -> f x : xs) []
(.) f g x = f (g x)
return x = [x]
join m k = foldr ((++) . k) [] m -- This is really >>= for the list monad

{-# SUPERCOMPILE root #-}
root f xs = (map f xs, join xs (return . f))
