-- Time-stamp: <Sun Mar 10 1996 23:04:57 Stardate: [-31]7179.80 hwloidl>
-----------------------------------------------------------------------------

#if defined(GRAN) || defined(GUM)

module Main(mainPrimIO) where

import PreludeGlaST

#else

module Main(main) where

#endif

-- import Random  (randomInts)    -- Just for testing
import ParForce

--import PreludeMonadicIO
--import PreludeIOError
-- import LibTime 

import Prog (prog)

-- main _ = [ReadChan stdin, AppendChan stdout (prog "")]

#if defined(GRAN) || defined(GUM)

#ifdef PRINT
mainPrimIO = getArgsPrimIO  `thenPrimIO` \ a ->
             let 
               all_args = args_to_IntList a     
               echo_str = prog (head all_args) ""
             in 
	       appendChanPrimIO stdout ("\nDecaffination argument (dummy!): " ++ (show (head all_args))) `seqPrimIO`
	     appendChanPrimIO stdout ("\nEcho str: " ++ (show echo_str)) `seqPrimIO`
	     returnPrimIO ()
#else
mainPrimIO = getArgsPrimIO  `thenPrimIO` \ a ->
             let 
               all_args = args_to_IntList a     
               echo_str = prog (head all_args) ""
             in 
	       if sum (map ord echo_str) == 13
		  then error "Qu'vatlh"
		  else returnPrimIO ()
#endif

#else /* no PrimIO i.e. std Haskell 1.2 */

#ifdef PRINT
main = getArgs exit ( \ a ->
             let 
               all_args = args_to_IntList a     
               echo_str = prog (head all_args) ""
             in 
	       appendChan stdout ("\nDecaffination argument (dummy!): " ++ (show (head all_args))) exit $ 
	     appendChan stdout ("\nEcho str: " ++ (show echo_str)) exit done )
#else
main = getArgs exit ( \ a ->
             let 
               all_args = args_to_IntList a     
               echo_str = prog (head all_args) ""
             in 
	       if sum (map ord echo_str) == 13
		  then error "Qu'vatlh"
		  else appendChan stdout "done" exit done)
#endif

#endif

args_to_IntList a = map (\ a1 -> fst ((readDec a1) !! 0)) a

-----------------------------------------------------------------------------

{-
	      max = 65535
	      randomList = map (`mod` max) (randomInts  (getRandInt max) 
							(getRandInt (max-1)) )

	      l' = take 5000 randomList  -- [54,53..2]::[Int]
	      l = par_qsort expensive_le l'

	      expensive_le x y = (sum [x..x+3]) <= (sum [y..y+3])

	      sorted l = and (zipWith (<=) l (tail l))

getRandInt :: Int -> Int
getRandInt bound = 
  unsafePerformPrimIO ( 
    getClockTime `thenPrimIO` \ t ->
    returnPrimIO (
    case t of 
    	Left _ -> error "error in getClockTime"
    	Right b -> let 
    		     CalendarTime _ _ _ _ _ _ x _ _ _ _ _  = toCalendarTime b
    		   in 
    		     ((fromInteger x) `mod` bound) :: Int )  )


-}