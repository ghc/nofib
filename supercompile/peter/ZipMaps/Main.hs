module Main where

main :: IO ()
main = print (root [1, 2, 3])

{-# SUPERCOMPILE root #-}
root xs = zip (map (\x -> Left x) xs) (map (\x -> Right x) xs)
