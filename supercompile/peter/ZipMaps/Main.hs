module Main where

import System.Environment

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (root [1..n])

{-# SUPERCOMPILE root #-}
root :: [a] -> Int
root xs = length $ zip (map (\x -> Left x) xs) (map (\x -> Right x) xs)
