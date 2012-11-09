module Main where

import Prelude hiding (length)

main :: IO ()
main = print (length "Hello World")
foldl' c n xs = case xs of [] -> n; (x:xs) -> let n' = c n x in n' `seq` foldl' c n' xs
{-# SUPERCOMPILE length #-}
length :: [a] -> Int
length xs = foldl' (\len _ -> len + 1) 0 xs
