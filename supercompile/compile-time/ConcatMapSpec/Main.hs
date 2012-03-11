module Main where

main :: IO ()
main = print (length (f1 0))

{-# SUPERCOMPILE f1 #-}
f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12 :: Int -> [Int]
f1 x = concatMap f2 [y, y + 1]
  where y = (x + 1) * 2
f2 x = concatMap f3 [y, y + 1]
  where y = (x + 1) * 2
f3 x = concatMap f4 [y, y + 1]
  where y = (x + 1) * 2
f4 x = concatMap f5 [y, y + 1]
  where y = (x + 1) * 2
f5 x = concatMap f6 [y, y + 1]
  where y = (x + 1) * 2
f6 x = concatMap f7 [y, y + 1]
  where y = (x + 1) * 2
f7 x = concatMap f8 [y, y + 1]
  where y = (x + 1) * 2
f8 x = concatMap f9 [y, y + 1]
  where y = (x + 1) * 2
f9 x = concatMap f10 [y, y + 1]
  where y = (x + 1) * 2
f10 x = concatMap f11 [y, y + 1]
  where y = (x + 1) * 2
f11 x = concatMap f12 [y, y + 1]
  where y = (x + 1) * 2
f12 x = [x + 1]
