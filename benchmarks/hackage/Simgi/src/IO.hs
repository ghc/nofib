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

-- | IO routines
module IO ( close_handle
          , get_handle
          , write_to_handle
          , FileHandle(..)
          ) where

-- imports
import Prelude
import System.IO
import Text.Printf


-- local imports
import GenericModel


-- | our custom Handle type; contains a "real" file handle
-- if the user requested output and otherwise None to indicate
-- that no output is supposed to be generated
data FileHandle = None | RealHandle Handle 



-- | close file handle if it is real
close_handle :: FileHandle -> IO ()
close_handle h = case h of
                   None              -> return ()
                   RealHandle handle -> hClose handle



-- | get a file handle if a file name is defined
get_handle :: String -> IO FileHandle
get_handle name = if name /= ""
                    then (openFile name WriteMode
                         >>= \handle -> return (RealHandle handle) )
                    else return None



-- | basic routine writing the simulation output to the 
-- file handle corresponding to the output file
write_to_handle :: FileHandle -> [Output] -> IO ()
write_to_handle _ []   = return ()
write_to_handle None _ = return ()
write_to_handle (RealHandle handle) ((Output {outputData = out}):xs) = 

  let 
    counts = foldr (\x a -> (printf "%18.2f  " x) ++ a) "" out
  in
    hPutStrLn handle counts
    >> write_to_handle (RealHandle handle) xs
