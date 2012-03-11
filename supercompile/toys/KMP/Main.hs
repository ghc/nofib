module Main where

main :: IO ()
main = print (root [A, A, B, A])

alphabetEq x y = case x of
    A -> case y of A -> True
                   _ -> False
    B -> case y of B -> True
                   _ -> False


match p s = loop p s p s

loop pp ss op os = case pp of
    []     -> True
    (p:pp) -> case ss of []     -> False
                         (s:ss) -> if alphabetEq p s then loop pp ss op os else next op os

next op ss = case ss of
    []     -> False
    (_:ss) -> loop op ss op ss

{-# SUPERCOMPILE root #-}
root u = match [A, A, B] u

data Alphabet = A | B