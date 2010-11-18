{-----------------------------------------------------------------
 
  (c) 2009-2010 Markus Dittrich 
 
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

-- | main gsim driver
module Main where

-- imports
import Prelude
import System.Environment

-- local imports
import CommandLine
import Engine
import InputCheck
import InputParser
import IO
import Messages
import TokenParser

-- import Debug.Trace


-- | main
main :: IO ()
main = 

  -- process command line arguments
  getArgs >>= process_commandline initialModelState
  >>= \(state, files) -> 


  -- reject anything but a single input file
  if length files /= 1 
    then usage
    else

      -- read input file and extract content
      readFile (head files)
      >>= \content -> 

        -- parse input file
        case runParser input_parser state "" content of
          Left er           -> putStrLn (show er)
          Right parsedState -> 
            case check_input parsedState of
              Left err -> putStrLn err
              Right _  -> 

                -- set up simuation
                let 
                  initialOutput = create_initial_output parsedState
                  initialState  = create_initial_state parsedState 
                                   initialOutput
                  totalTime     = maxTime parsedState 
                  dataDumpIter  = outputBufferSize parsedState
                  outFile       = outfileName parsedState
                in

                  get_handle outFile
                  >>= \handle -> 

                  -- print initial startup info
                  startup_message initialState

                  -- ready to run the simulation
                  >> gillespie_driver handle totalTime dataDumpIter 
                                  initialState 

                  >> close_handle handle
