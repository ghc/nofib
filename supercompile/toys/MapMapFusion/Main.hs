module Main where

import System.Environment

--import Prelude hiding (map)

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (root [1..(n :: Int)])

{-
map f xs = case xs of
    [] -> []
    (x:xs) -> f x : map f xs
-}

{-# SUPERCOMPILE root #-}
root :: [a] -> Int
root xs = length (map Left (map Right xs))
