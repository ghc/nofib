-- Time-stamp: <Wed Mar 21 2001 17:08:29 Stardate: [-30]6363.57 hwloidl>
-- 
-- tak benchmark program 
-- Divide-and-conquer structure with tertiary parallelism.
-----------------------------------------------------------------------------

#if 0 

-- Currently unused; should go into another dir!

#if defined(GRAN) || defined(PAR)
module Main(mainPrimIO) where
import PreludeGlaST
#else
module Main(main) where
#endif

import Parallel 

#if defined(GRAN) || defined(PAR)
# ifdef ARGS
mainPrimIO = getArgsPrimIO  `thenPrimIO` \ a ->
             munch_input a
args_to_IntList a = map (\ a1 -> fst ((readDec a1) !! 0)) a
# else
mainPrimIO = munch_input []      
# endif
#else  /* e.g. HBCPP */
# ifdef ARGS
main = getArgs exit ( \ a -> munch_input a )
args_to_IntList a = map (\ a1 -> fst ((readDec a1) !! 0)) a
# else
main = munch_input []      
# endif
#endif

#if defined(GRAN) || defined(PAR)
# ifdef PRINT
munch_input a = appendChanPrimIO stdout ("\ntak " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " = " ++ (show (tak x y z)) ++ "\n") `seqPrimIO`
	        returnPrimIO ()
# else
munch_input a = if (tak x y z) == 0
		then error "Qu'vatlh"
		else returnPrimIO ()
# endif
#else /* e.g. HBCPP */
# ifdef PRINT
munch_input a = appendChan stdout ("\ntak " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " = " ++ (show (tak x y z)) ++ "\n") exit done
# else
munch_input a = if (tak x y z) == 0
		then error "Qu'vatlh"
		else appendChan stdout "Ok" exit done
# endif
#endif

#ifdef ARGS
               where x =  (args_to_IntList a) !! 0
                     y =  (args_to_IntList a) !! 1
                     z =  (args_to_IntList a) !! 2
#else
               where x = 14
                     y = 8
                     z = 4	       
#endif

tak :: Int -> Int -> Int -> Int
tak x y z 
    | x <= y     = z
    | otherwise  = par x' (
                    par y' (
                     par z' (
                       res
                   ) ) )
                   where res = tak x' y' z'
                         x' = tak (x-1) y z
                         y' = tak (y-1) z x
                         z' = tak (z-1) x y
                         g =  gran x y z	 
                         gran x y z = abs (z-y) + abs (y-x) + abs (z-x)


#endif