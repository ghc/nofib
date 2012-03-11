module Main where

main :: IO ()
main = print (root (+) 0 1 10)

-- Example from Section 5 of "Shortcut Fusion for Accumulating Parameters & Zip-like Functions"
-- Optimal output should be isomorphic to:
--
--  root c n a b = foldDU n a
--    where
--      foldDU n a = if a > b
--                   then n
--                   else foldlDU (c n a) (a + 1)
--
-- In the course of supercompilation, we get here:
--
--    D[root c n a b]
--  ==>
--    if a > b
--    then n
--    else D[foldl c (c n a) (enumFromTo'Int (a + 1) b)]
--
-- If we generalise away the (c n a) and (a + 1) then we'll get a nice tieback to what we started
-- with that and hence build something equivalent to the input code.
{-# SUPERCOMPILE root #-}
root :: (Int -> b -> b) -> b -> Int -> Int -> b
root c n a b = foldl c n (enumFromTo a b)
