module Main where

data Burble a = B1 { op1 :: a -> Int, op2 :: Int -> a, op3 :: Int}
	      | B2 { op2 :: Int -> a, op4 :: Int -> Int } 


f1 :: Int -> Burble Int
f1 n = B1 { op1 = \x->x+n, op2 = \x -> x, op3 = n }

f2 :: Burble a -> Int -> Int
f2 (B1 {op1, op2}) n = op1 (op2 n)

f3 :: Burble a -> Int -> Int
f3 burb n = op1 burb (op2 burb n)

f4 :: Burble [Int] -> Burble [Int]
f4 x@(B1 {op1}) = x {op2 = \x->[x]}

main = print (f2 (f1 3) 4)
