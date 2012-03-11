module Main where

main :: IO ()
main = print (take 4 (fst root), take 4 (snd root))

example1 = let ones = 1 : ones
           in map (\x -> x + 1) ones

example2 = map (\x -> x + 1) (repeat 1)

{-# SUPERCOMPILE root #-}
root = (example1, example2)
