module Main(main) where

import Distributed
import System
import IO

	
data Link a = Link (MVar a) (MVar ())

instance Immobile (Link a) where 
  owningPE (Link i o) = owningPE i

newLink :: IO (Link a)
newLink = do
	i <- newEmptyMVar
	o <- newMVar ()
	return (Link i o)

readLink :: Link a -> IO a
readLink (Link i o) = do
	v <- takeMVar i
	putMVar o ()
	return v

writeLink :: Link a -> a -> IO ()
writeLink (Link i o) v = do
	takeMVar o
	putMVar i v

		
main =	do 
	putStrLn "Chain..."
	pes <- allPEId

	o <- newLink
	
	let spawn p = do
		i <- newLink
		let work = do
			v <- readLink i
			writeLink o ((v*2),p)
			work
		forkIO work
		return i

	is <- mapM (\p -> revalIO (spawn p) p) pes

	let 
	  work 0 = return True
	  work n = do
		writeLink (is!!(n `mod` (length is))) n
		(v,p) <- readLink o
		if v==(n*2)
		  then do
			putStrLn "Okay"
			work (n-1)
		  else do
			putStrLn ("ERROR: "++show p++" says 2*"++show n++"="++ show v)
			return False

	rs <- work ((length pes)*6)
	if rs 
          then putStrLn "Test PASSED" 
          else putStrLn "Test FAILED"
        return ()

{-

*Creates a slave thread on each PE which has the job of reading a number and returning
 double the result.

*Tests MVar communication, and thread creation.

-}	