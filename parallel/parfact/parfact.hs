-- Time-stamp: <Sat Jul 20 1996 21:27:16 Stardate: [-31]7839.26 hwloidl>
--
-- parfact
-- parallel version of a factorial-like function (i.e. divide-and-conquer)
-- using Glaswegian IO to minimize IO overhead.
-- To be used for GranSim
-----------------------------------------------------------------------------

module Main(main) where

import Parallel

main = getArgs exit ( \ args ->
       munch_input (args_to_IntList args) )

munch_input [n] = appendChan stdout ("\nparfact " ++ (show n) ++ " = " ++ (show (parfact n)) ++ "\n") exit done

args_to_IntList a = if length a < 1
		      then error "Usage: parfact <n>\n"
		      else map (\ a1 -> fst ((readDec a1) !! 0)) a

parfact :: Int -> Int
parfact x = pf 100 1 x

pf :: Int -> Int -> Int -> Int
pf n x y
   | x < y     = par f1 ( seq f2 (f1+f2) )
                 {-
		   _parGlobal_ p' p' 1# p' f1 
                      (_seq_ f2 (f1+f2))
                 -}
   | otherwise = x
   where
      m  = (x+y) `quot` 2   -- changed div to quot -> much more efficient   HWL
      f1 = pf (n-1) x m
      f2 = pf (n-1) (m+1) y
      -- p' = case (n) of (I# p') -> p'

