{-
 - Compress.hs
 - 
 - This program is a version of the compress utility as defined in
 - "A Technique for High Performance Data Compression", Terry A. Welch,
 - Computer, vol 17, no 6 1984, pp 8-19
 -
 - Usage: compress file
 -
 - Paul Sanders, Systems Research Division, British Telecom Laboratories 1992
 -
 -}

module Main (main) where

import Defaults   
import BinConv	  -- binary conversion routines
import Encode     -- coding routine


main = readChan stdin abort (\ inp ->
          appendChan stdout (compress inp) abort done)

{- To compress a string we first encode it, then convert it to n-bit binaries
 - convert back to decimal as ascii-bit values and then to characters
 -}

compress = map chr . codes_to_ascii . encode
