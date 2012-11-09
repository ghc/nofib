module Main where

import Prelude hiding (foldl)

main :: IO ()
main = print (root [1, 2, 3])

foldl c n xs = case xs of []     -> n
                          (y:ys) -> foldl c (c n y) ys

root :: [Int] -> Int
root xs = foldl (+) 0 xs
