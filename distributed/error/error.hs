module Main(main) where

import Distributed
import System
import Exception
		
main =	do  
	putStrLn "Error test..."
	pes <- allPEId
	m <- newEmptyMVar

	let 
	  work p = catchAllIO (revalIO remote p) fails	  
	  remote = do
		i  <- myPEId
		mo <- owningPE m
		putMVar m (if i==mo then "Owner" else "Other")
		return True
	  fails PutFullMVar = do
		name <- takeMVar m
		putStrLn ("Writer= "++name)
		return True
	  fails e = do
		putStrLn ("Error: "++show e)
		return False

	rs <- mapM work pes	
	catchAllIO (putMVar m "Main") (\e-> return ())
	name <- takeMVar m
	putStrLn ("Writer= "++name)

	if rs==(replicate (length pes) True) 
          then putStrLn "Test PASSED" 
          else putStrLn "Test FAILED"


{-

* tests the handling of remote exceptions.

-}