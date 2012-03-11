module Main where

main :: IO ()
main = print (root [1, 2])

eqList xs ys = case xs of []      -> case ys of []      -> True
                                                (y:ys') -> False
                          (x:xs') -> case ys of []      -> False
                                                (y:ys') -> if x == y then eqList xs' ys' else False

{-# SUPERCOMPILE root #-}
root xs = (xs ++ []) `eqList` xs
