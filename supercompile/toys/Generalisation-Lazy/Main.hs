module Main where

import Prelude hiding (foldl, length)

main :: IO ()
main = print (length "Hello")

foldl c n xs = case xs of [] -> n; (x:xs) -> foldl c (c n x) xs
{-# SUPERCOMPILE length #-}
length xs = foldl (\len _ -> S len) Z xs

data Nat = S Nat | Z
         deriving (Eq, Show)
