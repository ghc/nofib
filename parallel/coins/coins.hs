-- Time-stamp: <Sat Jul 20 1996 21:47:15 Stardate: [-31]7839.33 hwloidl>
--
-- Compute all ways in which a certain amount of money can be paid by using 
-- a fixed set of coins. In a NUM setup only the number of such possibilities
-- is computed. Use a GRAN setup for compilation under GranSim (i.e. compile 
-- time option -DGRAN), a GUM setup to get a real parallel program.
-- This is a pre-strategy version using forcing functions from the module
-- ParForce.hs
-----------------------------------------------------------------------------

#if defined(GUM) || defined(GRAN)

module Main(mainPrimIO) where

import PreludeGlaST

#else

module Main(main) where

#endif

-- import Random  (randomInts)    -- Just for testing

-- ToDo: Move this into the ParForce module to hide GUM/GrAnSim specifics
#if defined(GUM)
-- Ignore name and priority fields in GUM
parGlobal :: Int -> Int -> Int -> Int -> a -> b -> b
parGlobal _ _ _ _ = par

-- Copied from ParForce 
seq :: a -> b -> b
seq = _seq_

par :: a -> b -> b
par = _par_

par_map :: Int -> (a -> b) -> [a] -> [b]
par_map p f []     = []
par_map p f (x:xs) = parGlobal p p 1 0 fx 
				( parGlobal p p 1 0 (forcelist pmxs)  
						    (fx:pmxs) )
                  where fx = f x
			pmxs = par_map p f xs
		  
forcelist [] = ()
forcelist (x:xs) = seq x (forcelist xs)
#elif defined(GRAN)
import ParForce
#else /* HBCPP */
import ParForce
#endif

-- import PreludeMonadicIO
-- import PreludeIOError
-- import LibTime 

-- import Util   -- quicksort is in there (part of libghc)


#if defined(MAX_PAR)

pay_num :: Int -> Int -> [Int] -> Int
pay_num _ 0 coins    = 1 -- [accum]
pay_num _ val []     = 0
pay_num pri val coins  = 
    parGlobal 5 5 1 0 coins' (
     parGlobal 4 4 1 0 coins'' (
       res		
    ))        
    where coins'  = dropWhile (>val) coins
          coins'' = nub coins'      
          res = sum  ( par_map pri
                           ( \ c -> let 
                                      xs = dropWhile (>c) coins'
                                      new_coins = xs\\[c] 
                                    in 			   
                                     parGlobal 2 2 1 0 xs (
                                      parGlobal 3 3 1 0 new_coins (
     	                                pay_num (pri-1)
				                (val-c) 
                                                new_coins
                                    ) )
                           )
                           coins'' )

pay :: Int -> Int -> [Int] -> [Int] -> [[Int]]
pay pri 0 coins accum   = [accum]
pay pri val [] _        = []
pay pri val coins accum = 
    parGlobal 5 5 1 0 coins' (
     parGlobal 4 4 1 0 coins'' (
       res		
    ))        
    where coins'  = dropWhile (>val) coins
          coins'' = nub coins'      
          res = concat ( par_map pri
                           ( \ c -> let 
                                      new_coins = 
                                          ((dropWhile (>c) coins')\\[c])
                                    in 			   
                                     parGlobal 3 3 1 0 new_coins (
     	                              pay (pri-1)
				          (val-c) 
                                          new_coins
     	                                  (c:accum)
                                    )
                           )
                           coins'' )
#else
pay_num :: Int -> Int -> [Int] -> Int
pay_num _ 0   coins  = 1 -- [accum]
pay_num _ val []     = 0
pay_num pri val coins  = 
    res		
    where coins'  = dropWhile (>val) coins
          coins'' = nub coins'      
          res = sum ( par_map pri
                           ( \ c -> let 
                                      new_coins = 
                                          ((dropWhile (>c) coins')\\[c])
                                    in 			   
                                      pay_num  (pri-1)
				               (val-c) 
                                               new_coins
                           )
                           coins'' )

pay :: Int -> Int -> [Int] -> [Int] -> [[Int]]
pay _   0 coins accum   = [accum]
pay _   val [] _        = []
pay pri val coins accum = 
    res		
    where coins'  = dropWhile (>val) coins
          coins'' = nub coins'      
          res = concat ( par_map pri 
                           ( \ c -> let 
                                      new_coins = 
                                          ((dropWhile (>c) coins')\\[c])
                                    in 			   
                                      pay (pri-1)
				          (val-c) 
                                          new_coins
     	                                  (c:accum)
                           )
                           coins'' )
#endif


#if defined(RANDOM_INPUT)
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
#endif

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

count_results :: [[Int]] -> Int
count_results = sum . concat

#if defined(NUM)
jaH :: Int -> Int
jaH = id
#else
jaH :: [[Int]] -> Int
jaH  = length -- foldr (\ x y -> if (y==10) then 1 else 0) 0 
-- jaH = length . filter (==10) . concat 
#endif

#if defined(GRAN) || defined(GUM)

#ifdef ARGS
args_to_IntList a = map (\ a1 -> fst ((readDec a1) !! 0)) a

mainPrimIO = getArgsPrimIO  `thenPrimIO` \ a ->
             munch_input a
#else
mainPrimIO = munch_input []
#endif

#ifdef PRINT
munch_input a = appendChanPrimIO stdout ("\nCoins:\n" ++ (show coinsz)) 	`seqPrimIO`
	        appendChanPrimIO stdout ("\nValue: " ++ (show value)) 	`seqPrimIO`
	        appendChanPrimIO stdout ("\nResult: " ++ (pp res))	        `seqPrimIO`
	        appendChanPrimIO stdout "\n"                               `seqPrimIO`
	        returnPrimIO ()
#else

# ifdef ARGS
munch_input a = if null a
                  then 	appendChanPrimIO stdout usage  `seqPrimIO`
	                returnPrimIO ()    
                  else			
# else
munch_input _ = 
# endif
                        seq (jaH res) (returnPrimIO ())	
#endif

#else  /* no PrimIO i.e. std Haskell 1.2 */

#ifdef ARGS
args_to_IntList a = map (\ a1 -> fst ((readDec a1) !! 0)) a

main = getArgs exit ( \ a -> munch_input a )
#else
main = munch_input []
#endif

#ifdef PRINT
munch_input a = appendChan stdout ("\nCoins:\n" ++ (show coinsz)) abort $
	        appendChan stdout ("\nValue: " ++ (show value))   abort $
	        appendChan stdout ("\nResult: " ++ (pp res))	  abort $
	        appendChan stdout "\n"                            abort done
#else

# ifdef ARGS
munch_input a = if null a
                  then 	appendChan stdout usage abort done
                  else			
# else
munch_input _ = 
# endif
                        seq (jaH res) (appendChan stdout "done" abort done)
#endif

#endif  /* PrimIO? */
	     where 
#ifdef ARGS
                   -- usage = "Usage: coins <value> <coin1> <qty1> <coin2> <qty2> ...\n" 
                   usage = "Usage: coins <value> \n"

                   value = head (args_to_IntList a) -- 14
                   {-
                   coins_flat = tail (args_to_IntList a) -- 14		   
                   
                   zipify [] = []
                   zipify (c:q:xs) = (c,q) : zipify xs

                   coinsz = zipify coins_flat		   
                   coins = concat (map (\(v,q) -> [v | i <- [1..q]]) coinsz)   
                   -} 
#else
#if defined(RANDOM_INPUT)
                   value = (getRandInt 100) + 150 -- i.e. [150, 250]
#else    
                   value = 179 -- 279
#endif
#endif
                   vals = [250, 100, 25, 10, 5, 1]   
                   -- quants = [1, 3, 2, 5, 7, 12]		   -- std setup
                   quants = [5, 8, 8, 9, 12, 17]		   

		   coinsz = zip vals quants
                   coins = concat (map (\(v,q) -> [v | i <- [1..q]]) coinsz)

#if defined(NUM)
                   res = pay_num 100 value coins
#else
	           res = pay 100 value coins []
#endif


#if defined(NUM)
                   pp :: Int -> String    
                   pp = show	
#else
                   pp = unlines . reverse . snd . 
		         foldr (\ l (n,q) -> 
		                 (n+1, ("<" ++ (show (n, length l, sum l)) ++ "> " ++ (show l)):q )) 
				 (1,[])
#endif

