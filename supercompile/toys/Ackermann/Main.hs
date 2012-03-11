module Main where

import System.Environment

main :: IO ()
main = do
    [x_str] <- getArgs
    print (root (read x_str))

ack :: Int -> Int -> Int
ack m n = case m of 0 -> n + 1
                    _ -> case n of 0 -> ack (m-1) 1
                                   _ -> ack (m-1) (ack m (n-1))

{-# SUPERCOMPILE root #-}
root x = ack 2 x
