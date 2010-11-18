{-# LANGUAGE TypeOperators #-}
module TestData 
	( genPointsUniform
	, genPointsDisc
	, genPointsCombo
	, toPArrayPoints )
where

import qualified Types as QH
import qualified Data.Array.Parallel.Unlifted 	    as U
import qualified Data.Array.Parallel.Prelude 	    as P
import qualified Data.Array.Parallel.Prelude.Double as D
import qualified Data.Array.Parallel.PArray         as P
import Data.Array.Parallel.PArray		    (PArray)
import Data.Array.Parallel.Base ( (:*:)(..) )

import System.Random
import Control.Exception

-- Random points generation
-- IMPORTANT: We use the same seed with the same random generator in all
--            quickhull codes.  The asymptotic work complexity of quickhull
--            is between O (N) and O (N^2) depending on the input.
--            To compare benchmark results, they always need to use the same
--            input.
seed 		= 42742

-- | Some uniformly distributed points
genPointsUniform 
	:: Int			-- ^ number of points
	-- -> Double		-- ^ minimum coordinate
	-- -> Double		-- ^ maximum coordinate
        -> U.Array (Double :*: Double)
	-- -> [(Double, Double)]

genPointsUniform n -- minXY maxXY
 = let
	pointMin	= 50
	pointMax	= 150
	gen		= mkStdGen seed
        pts             = U.randomRs (n*2) (pointMin, pointMax) gen
        xs              = U.extract pts 0 n
        ys              = U.extract pts n n
   in
   U.zip xs ys

-- | Some points distributed as a disc
genPointsDisc'
	:: Int			-- ^ number of points
	-> (Double, Double) 	-- ^ center of disc
	-> Double 		-- ^ radius of disc
        -> U.Array (Double :*: Double)
	-- -> [(Double, Double)]

genPointsDisc' n (originX, originY) radiusMax
 = let	(genRadius, genAngle)		
		= split $ mkStdGen seed
	
	-- radius	= take n $ randomRs (0, radiusMax) genRadius 
	-- angle	= take n $ randomRs (- pi, pi) genAngle
	radius = U.randomRs n (0, radiusMax) genRadius
        angle  = U.randomRs n (-pi,pi) genAngle

	makeXY r a	
	 	= (originX + r * cos a)
	   	  :*: (originY + r * sin a)	
    in
    originX `seq` originY `seq` U.zipWith makeXY radius angle

genPointsDisc :: Int -> U.Array (Double :*: Double)
genPointsDisc n = genPointsDisc' n (150,150) 100

-- | A point cloud with areas of high an low density
genPointsCombo 
	:: Int 			-- ^ number of points
        -> U.Array (Double :*: Double)

genPointsCombo n
 	=  genPointsDisc' (n `div` 5) (250, 250) 200
	U.+:+ genPointsDisc' (n `div` 5) (100, 100) 80 
	U.+:+ genPointsDisc' (n `div` 5) (150, 300) 30 
	U.+:+ genPointsDisc' (n `div` 5) (500, 120) 30 
	U.+:+ genPointsDisc' (n `div` 5) (300, 200) 150

-- | Convert a list of points to a PArray
toPArrayPoints :: U.Array (Double :*: Double) -> IO (PArray QH.Point)
toPArrayPoints ps
  = do
      let pts = QH.points (P.fromUArrPA' (U.fsts ps))
                          (P.fromUArrPA' (U.snds ps))
      evaluate $ P.nf pts
      return pts

