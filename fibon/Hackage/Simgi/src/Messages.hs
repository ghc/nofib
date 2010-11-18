{-----------------------------------------------------------------
 
  (c) 2009-2010 Markus Dittrich,
      National Resource for Biomedical Supercomputing &
      Carnegie Mellon University
 
 
  This program is free software; you can redistribute it 
  and/or modify it under the terms of the GNU General Public 
  License Version 3 as published by the Free Software Foundation. 
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License Version 3 for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330, 
  Boston, MA 02111-1307, USA.

--------------------------------------------------------------------}

-- | message routines used in simgi
module Messages ( show_version 
                , startup_message
                , usage
                ) where


-- imports
import Prelude


-- local imports
import GenericModel


-- | show version info
show_version :: IO ()
show_version = putStrLn "This is simgi v0.3 (c) 2009-2010 Markus Dittrich"



-- | show a brief startup message
startup_message :: ModelState -> IO ()
startup_message state = show_version
  >> (putStrLn $ "\n-------- Simulation parameters ----------")
  >> (putStrLn $ "rng seed              : " ++ (show simSeed))
  >> (putStrLn $ "max time              : " ++ (show simTime) ++ " s")
  >> (putStrLn $ "system volume         : " ++ finalSimVol)
  >> (putStrLn $ "data output frequency : " ++ (show simFreq))
  >> (putStrLn $ "log output frequency  : " ++ (show simLogFreq))
  >> (putStrLn $ "output filename       : " ++ simOutFile)
  >> (putStrLn $ "-----------------------------------------")
  >> putStrLn "\nstarting simulation ...\n"


  where
    (ModelState { seed             = simSeed 
                , maxTime          = simTime
                , systemVol        = simVol
                , outputBufferSize = simLogFreq
                , outputFreq       = simFreq
                , outfileName      = simOutFile 
                }) = state

    -- for nil volumes we want to display nil not -1.0
    finalSimVol = if (simVol < 0) 
                    then "nil"
                    else (show simVol) ++ " dm^3"


-- | provide brief usage info
usage :: IO ()
usage = putStrLn "Usage: simgi [options] <input file>\n\n\
        \Currently supported options are:\n\n\
        \\t -s --seed <seed value> \n\
        \\t       specify value for the rng starting seed.\n\n\
        \\t -v --version-info \n\
        \\t       print version info.\n\n\
        \\t -h --help \n\
        \\t       print this help message."
