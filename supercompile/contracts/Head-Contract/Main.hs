module Main where

main :: IO ()
main = print (my_head_check [1, 2])

-- my_head : {xs | not (null xs)} -> Ok
my_head xs = case xs of y:_ -> y

{-# SUPERCOMPILE my_head_check #-}
my_head_check xs = my_head (case not (null xs) of
                        True  -> xs
                        False -> error "UNR")
