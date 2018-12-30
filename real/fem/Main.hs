-- Glasgow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : main.hs                DATE : 13-3-1991                *
-- *                                                                    *
-- * CONTENTS : Main program of FEM.                                    *
-- *                                                                    *
-- **********************************************************************

import Database
import Vector
import Displacement
import Elemforce
import PrintSource
import Printuvwforce

import Control.Exception
import Control.Monad
import Data.Char
import System.Environment

-- | Using @salt xs@ on an loop-invariant @xs@ inside a loop prevents the
-- compiler from floating out the input parameter.
salt :: [a] -> IO [a]
salt xs = do
  s <- length <$> getArgs
  -- Invariant: There are less than 'maxBound' parameters passed to the
  --            executable, otherwise this isn't really @pure . id@
  --            anymore.
  pure (take (max (maxBound - 1) s) xs)

main = do
  s <- getContents
  (n:_) <- getArgs
  replicateM_ (read n) $
    -- salt s >>= putStr . process
    salt s >>= evaluate . hash . process

hash :: String -> Int
hash = foldr (\c acc -> ord c + acc*31) 0

process :: String -> String
process s = a
  where
    a  = source_data db ++
         uvwresult db uvwres ++
         forceresult db frc
    db = (idatabase s, rdatabase s)
    uvwres = uvw db
    frc    = forces db uvwres
