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


main :: [Response] -> [Request]

main = readChan stdin abort process

process :: [Char] ->  [Response] -> [Request]

process s =
	appendChan stdout a abort done
        where
		a  = source_data db ++
		     uvwresult db uvwres ++ 
		     forceresult db frc     
		db = (idatabase s, rdatabase s)
		uvwres = uvw db
		frc    = forces db uvwres

