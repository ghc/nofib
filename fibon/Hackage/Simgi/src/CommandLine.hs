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

-- | main gsim driver
module CommandLine ( process_commandline 
                   , SimgiOpt(..)
                   ) where


-- imports
import Data.Word
import Prelude
import System
import System.Console.GetOpt

-- local imports
import GenericModel
import Messages



-- | main driver for command line processing
process_commandline :: ModelState -> [String] 
                    -> IO (ModelState, [String])
process_commandline state args = 

  let 
    (actions, nonOpts, _) = getOpt RequireOrder options args
  in
    foldl (>>=) ( return [] ) actions >>= \opts ->

    let 
      newState = process_options opts state 
    in
      return (newState,nonOpts) 



-- | process all user provided commandline options and adjust
-- the model state accordingly
process_options :: [SimgiOpt] -> ModelState -> ModelState
process_options [] state     = state
process_options ( (SimgiOpt { cmdlRequest = req
                            , cmdlString  = val }):xs) state =  

  case req of 
    Seed -> process_options xs ( state { seed = parse_seed val } )
    _    -> process_options xs state -- ignore unknown requests

  
  where
    parse_seed aSeed = floor (read aSeed :: Double) :: Word64 



-- | data structure describing all commandline options we know of
data CmdlRequest = Seed 



-- | data structure for keeping track of a specific user provided
-- command line switch 
data SimgiOpt = SimgiOpt {
  cmdlRequest :: CmdlRequest,
  cmdlString  :: String
}



-- | available command line flags
options :: [OptDescr ([SimgiOpt] -> IO [SimgiOpt])]
options = [
  Option ['v'] ["version-info"] (NoArg version_info) 
         "show version information",
  Option ['h'] ["help"] (NoArg help_msg) "show help message",
  Option ['s'] ["seed"] (ReqArg seed_value "SEED") "seed value"
 ]



-- | extractor function for version info
version_info :: [SimgiOpt] -> IO [SimgiOpt]
version_info _ =
  do
    show_version
    exitWith ExitSuccess



-- | extractor function for help message
help_msg :: [SimgiOpt] -> IO [SimgiOpt]
help_msg _ =
  do
    usage
    exitWith ExitSuccess



-- | extract the seed value 
seed_value :: String -> [SimgiOpt] -> IO [SimgiOpt]
seed_value arg opt = 
  return ( (SimgiOpt { cmdlString = arg, cmdlRequest = Seed }):opt)

