module Main where

import System.Environment

--import Prelude hiding (map)

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (root (my_enum 1 n))

-- Prevent benchmark results being skewed because rewrite rules have access to more information than the SC does:
my_enum :: Int -> Int -> [Int]
my_enum n m | n > m     = []
            | otherwise = n : my_enum (n+1) m

{-
map f xs = case xs of
    [] -> []
    (x:xs) -> f x : map f xs
-}

{-# SUPERCOMPILE root #-}
root :: [a] -> Int
root xs = length (map Left (map Right xs))
