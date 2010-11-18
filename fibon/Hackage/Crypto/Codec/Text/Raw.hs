-----------------------------------------------------------------------------
-- |
-- Module      :  Coded.Text.Raw
-- Copyright   :  (c) Dominic Steinitz 2006
-- License     :  BSD-style (see the file ReadMe.tex)
--
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Codec.Text.Raw(
   hexdump,
   hexdumpBy,
   ) where
   
import Data.List
import Codec.Utils
import Numeric
import Text.PrettyPrint

split :: Int -> [a] -> [[a]]
split n xs = unfoldr (g n) xs

g :: Int -> [a] -> Maybe ([a],[a])
g n [] = Nothing
g n y  = Just (splitAt n y)

sh x | x < 16    = '0':(showHex x "")
     | otherwise = showHex x ""

type OctetsPerLine = Int

hexdump :: OctetsPerLine -> [Octet] -> Doc
hexdump n = 
   vcat . 
   map hcat . 
   map (intersperse colon) . 
   map (map (text . sh)) . 
   split n 

hexdumpBy :: String -> OctetsPerLine -> [Octet] -> Doc
hexdumpBy s n = 
   vcat . 
   map hcat . 
   map (intersperse (text s)) . 
   map (map (text . sh)) . 
   split n 
