module Matrix (
   Matrix(..),
   Vector(..),
   fplMatrix,
   createVector,
   newVector,
   amx,
   saxpy,
   saxmy,
   xmy,
   negx,
   innerProd
) where

import MutableArray
import ByteArray
import ST

type Vector s = MutableByteArray s Int
type Diagonal = ByteArray Int
type Matrix = (Int, Diagonal, Diagonal, Diagonal)

fplMatrix :: Int -> Matrix
fplMatrix size = (size, d0, d1, d2)
   where
      n = size * size
      d0 = al n (\i -> 4)
      d1 = al (n - 1) (\i -> if ((i + 1) `mod` size == 0) then 0 else (-1))
      d2 = al (n - size) (\i -> -1)

      al n f = runST (do
         a <- newDoubleArray (0 :: Int, n - 1)
         a <- al_ 0 a
         freezeDoubleArray a)
         where
            al_ i a
               | i >= n    = return a
               | otherwise = do
                  writeDoubleArray a i (f i)
                  al_ (i + 1) a

createVector :: [Double] -> ST s (Vector s)
createVector xs = do 
   a <- newDoubleArray (0 :: Int, length xs - 1)
   createVector_ xs 0 a
   where
      createVector_ []     i a = return a
      createVector_ (x:xs) i a = do
         writeDoubleArray a i x
         createVector_ xs (i + 1) a

newVector :: Vector s -> ST s (Vector s)
newVector v = newDoubleArray (p, q)
   where
      (p, q) = boundsOfMutableByteArray v

saxmy :: Double -> Vector s -> Vector s -> ST s (Vector s)
saxmy a x y =
   saxmy' 0 n x y
      where 
         n = div (sizeofMutableByteArray x) 8

         saxmy' :: Int -> Int -> Vector s -> Vector s -> ST s (Vector s)
         saxmy' i n x y
            | i >= n    = return x
            | otherwise = do
               xe <- readDoubleArray x i
               ye <- readDoubleArray y i
               writeDoubleArray x i (a * xe - ye)
               saxmy' (i + 1) n x y

saxpy :: Double -> Vector s -> Vector s -> ST s (Vector s)
saxpy a x y =
   saxpy' 0 n x y
      where
         n = div (sizeofMutableByteArray x) 8

         saxpy' :: Int -> Int -> Vector s -> Vector s -> ST s (Vector s)
         saxpy' i n x y
            | i >= n    = return y
            | otherwise = do
               xe <- readDoubleArray x i
               ye <- readDoubleArray y i
               writeDoubleArray y i (a * xe + ye)
               saxpy' (i + 1) n x y

xmy :: Vector s -> Vector s -> ST s (Vector s)
xmy x y =
   xmy' 0 n x y
      where
         n = div (sizeofMutableByteArray x) 8

         xmy' :: Int -> Int -> Vector s -> Vector s -> ST s (Vector s)
         xmy' i n x y
            | i >= n    = return x
            | otherwise = do
               xe <- readDoubleArray x i
               ye <- readDoubleArray y i
               writeDoubleArray x i (xe - ye)
               xmy' (i + 1) n x y 

negx :: Vector s -> Vector s -> ST s (Vector s)
negx x u =
   negx' 0 n x u
      where
         n = div (sizeofMutableByteArray x) 8

         negx' :: Int -> Int -> Vector s -> Vector s -> ST s (Vector s)
         negx' i n x u
            | i >= n    = return u
            | otherwise = do
               xe <- readDoubleArray x i
               writeDoubleArray u i (negate xe)
               negx' (i + 1) n x u

innerProd :: Vector s -> Vector s -> ST s Double
innerProd x y =
   innerProd' 0 0 n x y
      where
         n = div (sizeofMutableByteArray x) 8

         innerProd' :: Double -> Int -> Int -> Vector s -> Vector s -> ST s Double
         innerProd' r i n x y
            | i >= n    = return r
            | otherwise = do
                xe <- readDoubleArray x i
                ye <- readDoubleArray y i
                innerProd' (r + xe * ye) (i + 1) n x y

amx :: Vector s -> Matrix -> Vector s -> ST s (Vector s)
amx u (offset, d0, d1, d2) v = do
   u <- mul0 0 n d0 v u
   u <- mul1 0 1 n d1 v u
   u <- mul1 0 offset n d2 v u
   return u
      where
         n = div (sizeofMutableByteArray v) 8

--         mul0 :: Int -> Int -> Diagonal -> Vector s -> Vector s -> ST s (Vector s)
         mul0 i (n :: Int) d0 v u 
            | (i :: Int) >= n    = return u
            | otherwise = do
               ve    <- readDoubleArray v i
               let de = indexDoubleArray d0 i
               writeDoubleArray u i (de * ve)
               mul0 (i + 1) n d0 v u

--         mul1 :: Int -> Int -> Int -> Diagonal -> Vector s -> Vector s -> ST s (Vector s)
         mul1 i1 i2 (n :: Int) d v u
            | (i2 :: Int) >= n   = return u
            | otherwise = do
               let de = indexDoubleArray d i1
               e1    <- readDoubleArray u i1
               e2    <- readDoubleArray u i2
               ve1   <- readDoubleArray v i1
               ve2   <- readDoubleArray v i2
               writeDoubleArray u i1 (e1 + de * ve2)
               writeDoubleArray u i2 (e2 + de * ve1)
               mul1 (i1 + 1) (i2 + 1) n d v u
