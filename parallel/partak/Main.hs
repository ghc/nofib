-- Time-stamp: <Fri Mar 30 2001 16:45:36 Stardate: [-30]6408.28 hwloidl>
-- 
-- tak benchmark program 
-- Divide-and-conquer structure with tertiary parallelism.
-----------------------------------------------------------------------------

module Main(main) where

import System(getArgs)
import Parallel 

main = do args <- getArgs
          let 
            x = read (args!!0) :: Int
            y = read (args!!1) :: Int
            z = read (args!!2) :: Int
            res = tak x y z
          putStrLn ("\ntak " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " = " ++ (show res))

partak :: Int -> Int -> Int -> Int
partak x y z 
    | x <= y     = z
    | otherwise  = x' `par` y' `par` z' `par`
                   res
                   where res = tak x' y' z'
                         x' = tak (x-1) y z
                         y' = tak (y-1) z x
                         z' = tak (z-1) x y
                         -- g =  gran x y z	 
                         -- gran x y z = abs (z-y) + abs (y-x) + abs (z-x)
