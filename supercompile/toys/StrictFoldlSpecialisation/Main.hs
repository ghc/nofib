module Main where

import Prelude hiding (foldl')

main :: IO ()
main = print (root [1, 2, 3])

foldl' c n xs = case xs of []     -> n
                           (y:ys) -> let n' = c n y in case n' of _ -> foldl' c n' ys

{-# SUPERCOMPILE root #-}
root :: [Int] -> Int
root xs = foldl' (+) 0 xs
