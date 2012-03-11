module Main where

import System.Environment

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (toInt (root peanoPlus Z (S Z) (fromInt n)))

-- Example from Section 5 of "Shortcut Fusion for Accumulating Parameters & Zip-like Functions", converted to Peano.
-- Optimal output should be isomorphic to:
--
--  root c n a b = foldDU n a
--    where
--      foldDU n a = if a `peanoGt` b
--                   then n
--                   else foldlDU (c n a) (S a)
--
-- In the course of supercompilation, we get here:
--
--    D[root c n a b]
--  ==>
--    if a > b
--    then n
--    else D[foldl c (c n a) (enumFromTo (S a) b)]
--
-- If we generalise away the (c n a) and (S a) then we'll get a nice tieback to what we started
-- with that and hence build something equivalent to the input code.
{-# SUPERCOMPILE root #-}
root :: (Nat -> b -> b) -> b -> Nat -> Nat -> b
root c n a b = foldl c n (enumFromTo a b)

data Nat = S Nat | Z
         deriving (Eq, Show)

peanoGt :: Nat -> Nat -> Bool
peanoGt Z     _     = False
peanoGt (S _) Z     = True
peanoGt (S m) (S n) = peanoGt m n

peanoPlus :: Nat -> Nat -> Nat
peanoPlus Z     n = n
peanoPlus (S m) n = S (peanoPlus m n)

fromInt :: Int -> Nat
fromInt 0 = Z
fromInt n = S (fromInt (n - 1))

toInt :: Nat -> Int
toInt Z     = 0
toInt (S n) = 1 + toInt n
