--!!! the ultra-notorious "nfib 30" (overloaded arithmetic)
--
module Main (main) where

main = print (nfib 30)

nfib 0 = 1
nfib 1 = 1
nfib n = nfib (n - 2) + nfib (n - 1) + 1
