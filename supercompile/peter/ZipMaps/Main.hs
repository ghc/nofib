module Main where

import System.Environment

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (root (my_enum 1 n))

-- Prevent benchmark results being skewed because rewrite rules have access to more information than the SC does:
my_enum :: Int -> Int -> [Int]
my_enum n m | n > m     = []
            | otherwise = n : my_enum (n+1) m

{-# SUPERCOMPILE root #-}
root :: [a] -> Int
root xs = length $ zip (map (\x -> Left x) xs) (map (\x -> Right x) xs)
