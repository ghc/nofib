-- Now you want to recompile Channel,ChannelVar, Merge,SampleVar,Semaphore 
--  to use this module rather than the the Concurrent module...

-- I ignore the fact that the file handling stuff needs MVars too, so maybe
--   we should be replacing PrelConc in the longrun! The dependencies will be
--   fun to sort out though.

module Distributed
(	  PEId			-- abstract
        , Immobile		-- abstract class
	, myPEId		-- :: IO PEId
	, allPEId		-- :: IO [PEId]
	, owningPE		-- :: a -> IO PEId
	, revalIO		-- :: IO a -> b -> IO a -- (b is PEId, MVar, ThreadId)

	, on			-- :: a -> PEId -> a
	, mainPE		-- :: PEId

	, ThreadId 		-- abstract
	, killThread		-- :: ThreadId -> IO ()
	, raiseInThread		-- :: ThreadId -> Exception -> IO ()
	, yield			-- :: IO ()
	, threadDelay		-- :: Int -> IO ()
	, myThreadId 		-- :: IO ThreadId
	, forkIO		-- :: IO () -> IO ThreadId
	, rforkIO		-- :: IO () -> PEId -> IO ThreadId

	, MVar			-- abstract
	, newMVar		-- :: a -> IO (MVar a)
	, newEmptyMVar		-- :: IO (MVar a)
	, takeMVar		-- :: MVar a -> IO a
	, putMVar		-- :: MVar a -> a -> IO ()
	, readMVar		-- :: MVar a -> IO a
        , swapMVar		-- :: MVar a -> a -> IO a
        , isEmptyMVar		-- :: MVar a -> IO Bool
	, tryPutMVar		-- :: MVar a -> a -> IO Bool
	, takeMaybeMVar		-- :: MVar a -> IO (Maybe a) 
) where

import PrelDistributed

import Concurrent (ThreadId, MVar, forkIO, yield, threadDelay, myThreadId, newMVar, newEmptyMVar)
import qualified Concurrent
import qualified PrelConc

import Exception ( Exception(..),ArithException(..), AsyncException(..))
import qualified Exception

import PrelPack (packString)
import qualified PrelIOBase
import PrelAddr (Addr)

---------------------------
-- basic PEId stuff

myPEId :: IO PEId
myPEId = do
	p <- _ccall_ cGetMyPEId
	return (PEId p)

allPEId	:: IO [PEId]
allPEId = do
	(cnt :: Int) <- _ccall_ cGetPECount
	let getPE (n :: Int) = do
		p <- _ccall_ cGetPEId n
		return (PEId p)	
	mapM getPE [1..cnt]
	
instance Show PEId where
  showsPrec _ (PEId p) s = ("PE:"++(show p)) ++ s 
				
---------------------------
-- basic immobile resources stuff

class Immobile a where
  owningPE :: a -> IO PEId
  revalIO :: IO b -> a -> IO b

  revalIO job x = do
	p <- owningPE x
	doRevalIO job p 

  owningPE x = primGetCertainOwner x


-- General GpH Problems:
--  1 - signal handlers not installed... so can't catch div-by-zero yet
 
data Status a = Okay a | Fail Exception

doRevalIO :: IO a -> PEId -> IO a
doRevalIO job p = do
	i <- myPEId	
	if p==i
	  then job		-- keep it simple if its local
	  else do			
		primRevalIO result p 

		-- block until we know the result is Okay or not. 
		case result of
		  Okay r -> return r
		  Fail e -> Exception.throw e

	where 	
	  -- we wrap job up to return an 'okay' result.
	  okayJob = do 
		r <- job
		return (Okay r)

	  -- if something goes wrong we return a 'Fail' result.
	  safeJob = Exception.catchAllIO okayJob (\e -> return (Fail e))

	  -- we return the result via a single variable so we can
	  --  use GpH's synchronisation mechanisms ie FETCHME, BQ, etc. (hackity, hack.)
	  result = PrelIOBase.unsafePerformIO safeJob
	
---------------------------
-- utility functions

on :: a -> PEId -> a
on x p = PrelIOBase.unsafePerformIO (revalIO (x `seq` return x) p)

mainPE :: PEId
mainPE = (PEId (PrelIOBase.unsafePerformIO (_ccall_ cGetPEId (1::Int))))

---------------------------
-- immobile PEIds - well it fits in nicely :)

instance Immobile PEId where 
  owningPE p = return p 

---------------------------
-- immobile MVars 

instance Immobile (MVar a)

---------------------------
-- immobile ThreadIds

-- The big issue is should the reval thread have the same threadId as its parent?
-- I think we should say NO! If you want to know your parents Id then let the 
--   programmer pass it explicitly. Why make my life hard!
-- This opinion could require rethinking later...

instance Immobile ThreadId

---------------------------
-- immobile Host Names (RFP playing around)

instance Immobile ([Char]) where 
  owningPE h = do
	p <- _ccall_ cGetHostOwner (packString h)	
	case p of
	  0 -> error "no such host"
	  _ -> return (PEId p) 
	  
---------------------------
-- forking stuff

rforkIO 		:: IO () -> PEId -> IO ThreadId
rforkIO job p		= revalIO (forkIO job) p

---------------------------
-- killing

killThread		:: ThreadId -> IO ()
killThread th		= revalIO (Concurrent.killThread th) th

raiseInThread		:: ThreadId -> Exception -> IO ()
raiseInThread th ex	= revalIO (Concurrent.raiseInThread th ex) th

---------------------------
-- MVAR primitives  

takeMVar		:: MVar a -> IO a
takeMVar mv		= revalIO (Concurrent.takeMVar mv) mv

putMVar			:: MVar a -> a -> IO ()
putMVar	mv r		= revalIO (Concurrent.putMVar mv r) mv

readMVar		:: MVar a -> IO a
readMVar mv		= revalIO (Concurrent.readMVar mv) mv

swapMVar		:: MVar a -> a -> IO a
swapMVar mv r		= revalIO (Concurrent.swapMVar mv r) mv

isEmptyMVar		:: MVar a -> IO Bool
isEmptyMVar mv		= revalIO (Concurrent.isEmptyMVar mv) mv

tryPutMVar		:: MVar a -> a -> IO Bool 
tryPutMVar mv r		= Exception.catchAllIO (do;putMVar mv r;return True) (\e->return False) 

takeMaybeMVar		:: MVar a -> IO (Maybe a)
takeMaybeMVar mv	= revalIO (PrelConc.takeMaybeMVar mv) mv

