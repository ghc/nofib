-- Time-stamp: <Wed Mar 21 2001 17:09:08 Stardate: [-30]6363.57 hwloidl>
-- 
-- Good old nfib, now in parallel!
--
-----------------------------------------------------------------------------

#if 0

-- Currently unused; see ../pfib instead

module Main(main) where

import Parallel

main = getArgs exit ( \ args ->
       munch_input (args_to_IntList args) )

munch_input [n] = appendChan stdout  ("\nparfib " ++ (show n) ++ {- " (with CUT_OFF=" ++ (show CUT_OFF) ++ -} " = " ++ (show (parfib n)) ++ "\n") exit done

args_to_IntList a = if length a < 1
		      then error "Usage: parfib <n>\n"
		      else map (\ a1 -> fst ((readDec a1) !! 0)) a


nfib :: Int -> Int
nfib 0 = 1
nfib 1 = 1
nfib x = nfib (x-2) + nfib (x-1) + 1

parfib :: Int -> Int
parfib 0 = 1
parfib 1 = 1
parfib x = par  nf'' (seq nf'  (nf'+nf''+1) )
	    where nf'  = parfib (x-1)
		  nf'' = parfib (x-2)

parfib_cutoff :: Int -> Int
parfib_cutoff 0 = 1
parfib_cutoff 1 = 1
parfib_cutoff x 
 | x<CUT_OFF  = nfib x
 | otherwise  = par nf'' (seq nf'  (nf'+nf''+1) )
	        where nf'  = parfib_cutoff (x-1)
		      nf'' = parfib_cutoff (x-2)

par_non_fib :: Int -> Int -> Int -> Int
par_non_fib a b 0 = 1
par_non_fib a b 1 = 1
par_non_fib a b x = 
                    par ab  (
                    seq nf' (
                    seq nf'' (
		    (nf'+nf''+ab+1) ) ) )
		    where nf'  = a * (par_non_fib a' b' (x-1))
			  nf'' = b * (par_non_fib a' b' (x-2))
                          a' = max 1 (max (a-1) b)
                          b' = max 1 (min (a-1) b)
                          ab = gcd a' b'			  


#endif
