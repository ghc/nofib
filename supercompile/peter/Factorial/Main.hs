module Main where

import System.Environment

main :: IO ()
main = do
    [x] <- fmap (map read) getArgs
    print (fac x)

-- Integer arithmetic makes this rather boring, result is usually 0
{-# SUPERCOMPILE fac #-}
fac :: Int -> Int
fac n = case n == 0 of
    True -> 1
    False -> n * fac (n-1)
