--!!! the ultra-notorious "nfib 30"
--
module Main (main) where

main = print (nfib 30)

nfib :: Int -> Int
nfib n = if n <= 1 then 1 else nfib (n-1) + nfib (n-2) + 1
