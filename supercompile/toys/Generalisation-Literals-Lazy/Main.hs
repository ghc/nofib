module Main where

import Prelude hiding (length, foldl)

main :: IO ()
main = print (length "Hello World")
foldl c n xs = case xs of [] -> n; (x:xs) -> foldl c (c n x) xs
{-# SUPERCOMPILE length #-}
length :: [a] -> Int
length xs = foldl (\len _ -> len + 1) 0 xs
