
-- This version prints a SVG of the test data and computed hull to stdout.
-- usage:  vectSVG <POINTS>
--
import qualified Types as QH
import QuickHullVect (quickhull)

import qualified Data.Array.Parallel.Unlifted 	    as U
import qualified Data.Array.Parallel.Prelude 	    as P

import qualified Data.Array.Parallel.PArray         as P
import Data.Array.Parallel.PArray		    (PArray)

import System.Environment
import System.IO
import Data.List

import SVG
import TestData


-----
runQuickhull :: PArray QH.Point -> [(Double, Double)]
runQuickhull pts 
 = let result = quickhull pts
       resxs  = P.toUArrPA (QH.xsOf result)
       resys  = P.toUArrPA (QH.ysOf result)
   in  resxs U.!: 0 `seq` (zip (U.toList resxs) (U.toList resys))


-- Main Program ---------------------------------------------------------------
main 
 = do	[arg]	<- getArgs
        pts     <- case reads arg of
                     [(n,"")] -> return $ genPointsCombo n
                     _        -> do
                                   h <- openBinaryFile arg ReadMode
                                   pts <- U.hGet h
                                   hClose h
                                   return pts
        paInput <- toPArrayPoints pts

	let psHull  = runQuickhull paInput
	    psInput = P.toList paInput
	
	putStr 
	 $ makeSVG 
		(roundPoints psInput)
		(roundPoints psHull)
