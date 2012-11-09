module Main where

import Prelude hiding (map)

main :: IO ()
main = print (root [1, 2, 3 :: Int] :: [Either (Either Int Int) Int])

map f xs = case xs of
    [] -> []
    (x:xs) -> f x : map f xs

{-# SUPERCOMPILE root #-}
root :: [a] -> [Either (Either b a) c]
root xs = map Left (map Right xs)
