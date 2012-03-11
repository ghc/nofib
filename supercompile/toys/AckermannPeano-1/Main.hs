module Main where

main :: IO ()
main = print (root Z)

ack m n = case m of S m -> case n of S n -> ack m (ack (S m) n)
                                     Z   -> ack m (S Z)
                    Z   -> S n

{-# SUPERCOMPILE root #-}
root :: Nat -> Nat -> Nat
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