module Main where

import ST
import MutableArray
import Matrix

eps :: Double
eps = 1.0E-12

size :: Int
size = 25

size2 :: Int
size2 = size * size

main = print (runST (do
   let a = fplMatrix size
   x0 <- createVector (take size2 (repeat 1))
   b  <- createVector (take size2 (repeat (0.1)))
   x  <- cg a x0 b
   readDoubleArray x 0
   )) 

cg :: Matrix -> Vector s -> Vector s -> ST s (Vector s)
cg a x b = do
   d      <- newVector x
   h      <- newVector x
   g      <- newVector x
   g      <- amx g a x
   g      <- xmy g b
   delta0 <- innerProd g g
   if delta0 <= eps
      then
         return x
      else do
         d <- negx g d
         cgLoop a x g delta0 d h

cgLoop :: Matrix -> Vector s -> Vector s -> Double -> Vector s -> Vector s ->
   ST s (Vector s)
cgLoop a x g delta0 d h = do
   h      <- _scc_ "amx" amx h a d
   ip_dh  <- innerProd d h
   let tau = delta0 / ip_dh
   x1     <- _scc_ "saxpy1" saxpy tau d x
   g      <- _scc_ "saxpy2" saxpy tau h g
   delta1 <- innerProd g g
   if delta1 <= eps
      then
         return x
      else do
         let beta = delta1 / delta0
         d       <- _scc_ "saxmy" saxmy beta d g
         cgLoop a x1 g delta1 d h 
