module Main where
import Numbers
import Vectors
import Hide
import IO
import EdgePlate	( Input(..) ) -- partain
import Postscript	( Output(..) ) -- partain

main = readChan stdin abort
		(getFilename (process (\viewdir -> hiddenline viewdir. map read. lines)) .
		 lines)
