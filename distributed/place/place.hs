import Distributed
import System
import IO

main =	do 
	putStrLn "Place Test..."

	pes <- allPEId

	let
	  tot = length pes

	  check mv = do
		i <- myPEId
		o <- owningPE mv
		return (i,o)
 
	  same = map (\(x,y) -> x==y)
	
	  test n = (replicate n False)++[True]++(replicate (tot-1-n) False)

	  loop n = do
		let pe = pes!!n
		mv <- revalIO (newEmptyMVar) pe
		rs <- mapM (\p -> revalIO (check mv) p) pes
		if (same rs)==(test n)
		  then do
			putStrLn "Okay"
			return True
		  else do
			putStrLn ("ERROR: MVar should be located at "++show pe ++". Results "++show rs)	
			return False	
				
	rs <- mapM loop [0..tot-1]

	if rs==(replicate tot True) 
	  then putStrLn "Test PASSED" 
	  else putStrLn "Test FAILED"
	return ()

{-

* Creates an MVar on each PE and then checks that it can be correctly
  located from all other PEs.

* Tests owningPE on MVars.

-}