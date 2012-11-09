module Main where

main :: IO ()
main = print (root [1, 2, 3 :: Int])

{-# SUPERCOMPILE root #-}
root :: [a] -> [(Either a a, Either a a)]
root xs = zip (map (\x -> Left x) xs) (map (\x -> Right x) xs)
