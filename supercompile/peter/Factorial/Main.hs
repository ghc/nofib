module Main where

import System.Environment

main :: IO ()
main = do
    [x] <- fmap (map read) getArgs
    print (fac 1 x)

-- Integer arithmetic makes this rather boring, result is usually 0
{-# SUPERCOMPILE fac #-}
fac :: Int -> Int -> Int
fac acc n = case n == 0 of
    True -> acc
    False -> acc `seq` fac (acc * n) (n-1)
