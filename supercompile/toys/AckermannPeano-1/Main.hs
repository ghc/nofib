module Main where

import System.Environment

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (fromPeano 0 (root (toPeano n)))

toPeano :: Int -> Nat
toPeano 0 = Z
toPeano n = S (toPeano (n - 1))

fromPeano :: Int -> Nat -> Int
fromPeano n Z     = n
fromPeano n (S x) = n `seq` fromPeano (1 + n) x

ack m n = case m of S m -> case n of S n -> ack m (ack (S m) n)
                                     Z   -> ack m (S Z)
                    Z   -> S n

{-# SUPERCOMPILE root #-}
root :: Nat -> Nat
root x = ack (S Z) x

-- Optimal output:
--   root x = ackSZ x
--   ackSZ n = case n of Z   -> ackZ (S Z)
--                       S n -> ackZ (ackSZ n)
--   ackZ n = S n

-- Optimal output with some extra evaluation in ungeneralized branches:
--   root x = ackSZ x
--   ackSZ n = case n of Z   -> S (S Z)
--                       S n -> ackZ (ackSZ n)
--   ackZ n = S n

data Nat = Z | S Nat
         deriving (Eq, Show)