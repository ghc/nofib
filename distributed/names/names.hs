module Main(main) where

import Distributed
import System.Environment
import Data.List
import System.IO
import System.Time 
import PrelNum

-- we are only interested in ms accuracy < 60 seconds
runTime :: ClockTime -> ClockTime -> Int
runTime t1 t2 = 
	let
	  TimeDiff _ _ d h m s p = diffClockTimes t1 t2
	  (dp,mp) = divModInteger p 1000000000
	  b = integer2Int dp
	  (nb,es) = if(b<0) then (b+1000,s-1) else (b,s)
	  ns  = if(es<0) then es+60 else es
	in  nb+1000*ns 


timeit :: IO a -> IO (a, Int)
timeit job = do
	start <- getClockTime
	res <- job
	done <- getClockTime
	return (res,(runTime done start))
			
type PeNames = [(PEId,[String])]

host2pe :: PeNames -> String -> PEId
host2pe pns n = 
	case (filter (\(p,xs) -> elem n xs) pns) of
	  [(p,_)] -> p
	  _       -> error "unknown Host"

pe2host :: PeNames -> PEId -> String
pe2host pns t =
	case (filter (\(p,xs) -> p==t) pns) of
	  [(_,h:_)] -> h
	  _         -> error "unknown PEId"

peNames :: IO PeNames
peNames = do
	pes@(m:_) <- allPEId
	
	let work p = do
		n <- revalIO (getEnv "HOST") p
		return (p,n)

	ws <- mapM work pes
	
	let
	  us = map (\n -> (n,0)) (nub (map (\(_,n)->n) ws))

	  reduce = foldl (\acc (t,x) -> if t then (acc++[x]) else acc) []

	  unique (us,acc) (p,n) = (nus,acc++[(p,nn)])
		where 
		  (nus,nn) = loop us
		  loop ((x,c):t) = 
			if x==n
			  then ((x,c+1):t, reduce [(p==m,"MainPE"),(c==0,x),(True,(show c)++"@"++x)])
			  else let (rt,rn) = (loop t) in ((x,c):rt,rn)
	
	  (_,ns) = foldl unique (us,[]) ws
	return ns
	 
main =	do 
	pes <- allPEId

	putStrLn "PE Names..."
	ns <- peNames
	mapM (\x -> putStrLn (show x)) ns 

	putStrLn "\nTiming..."
	let	
	  timePing p = do
		putStr ("Pinging "++(pe2host ns p)++" ... ")
		(_,ms) <- timeit (revalIO (return ()) p)
		putStrLn (" time="++show ms++"ms")
	mapM timePing pes

	return ()


{-

* Generates unique and meaningful names for each PE, then times communication.

* Tests remote host lookup and timing.

-}
