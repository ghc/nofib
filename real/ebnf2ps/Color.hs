--                            -*- Mode: Haskell -*- 
-- Copyright 1994 by Peter Thiemann
-- Color.hs --- string converter for colors
-- Author          : Peter Thiemann
-- Created On      : Thu Dec  2 16:58:33 1993
-- Last Modified By: Peter Thiemann
-- Last Modified On: Fri Dec  3 14:13:34 1993
-- Update Count    : 3
-- Status          : Unknown, Use with caution!
-- 
-- $Locker:  $
-- $Log: Color.hs,v $
-- Revision 1.3  1999/01/18 19:38:46  sof
-- Misc (backward compatible) changes to make srcs acceptable
-- to a Haskell 98 compiler.
--
-- Revision 1.2  1996/07/25 21:23:51  partain
-- Bulk of final changes for 2.01
--
-- Revision 1.1  1996/01/08 20:02:35  partain
-- Initial revision
--
-- Revision 1.1  1994/03/15  15:34:53  thiemann
-- Initial revision
--
-- 

module Color where
-- (Color (..), lookupColor, showsColor, prepareColors)

import Char -- 1.3
import List ((\\)) -- 1.3

type Color = (Int, Int, Int)

noColor :: Color
noColor = (-1, -1, -1)

{-
readColor :: String -> Color
readColor =  readColor1 . map toLower

readColor1 :: String -> Color
readColor1 ('b':'l':'a':_) = 0
readColor1 ('b':'l':'u':_) = 1
readColor1 ('g':_)         = 2
readColor1 ('c':_)	   = 3
readColor1 ('r':_)         = 4
readColor1 ('m':_)         = 5
readColor1 ('y':_)	   = 6
readColor1 ('w':_)	   = 7
readColor1 _		   = -1
-}

lookupColor :: String -> [(String, (a, b, c))] -> (a, b, c)
lookupColor colorName colorTable =
	head [(r,g,b) | (c,(r,g,b)) <- colorTable, c == map toLower colorName]

showsColor :: Color -> ShowS
showsColor    (r,g,b) =  showString " (" . shows r . showChar ',' .
                                           shows g . showChar ',' .
					   shows b . showChar ')'

prepareColors rgbFile colors = 
	decodeColors (map (map toLower) colors) (fallBackRgb++parsedRgbFile) []
  where parsedRgbFile =  (map parseLine (lines rgbFile))

decodeColors [] parsedRgbFile decoded = decoded
decodeColors clrs [] decoded = [(name,(128,128,128)) | name <- clrs ]++decoded
decodeColors clrs ((r,g,b,name):parsedRgbFile) decoded
	= decodeColors (clrs \\ found) parsedRgbFile (foundDecoded++decoded)
	where found = [ c | c <- clrs, name == c ]
	      foundDecoded = [ (c,(r,g,b)) | c <- found ]

parseLine str = let (r,restr):_ = reads{-was:readDec-} (skipWhite str)
		    (g,restg):_ = reads{-was:readDec-} (skipWhite restr)
		    (b,restb):_ = reads{-was:readDec-} (skipWhite restg)
		    name = skipWhite restb
		in  (r,g,b,name)
  where skipWhite = dropWhile isSpace

fallBackRgb :: [(Int,Int,Int,String)]
fallBackRgb  = [
	(  0,  0,  0,"black"),
	(  0,  0,255,"blue"),
	(  0,255,  0,"green"),
	(  0,255,255,"cyan"),
	(255,  0,  0,"red"),
	(255,  0,255,"magenta"),
	(255,255,  0,"yellow"),
	(255,255,255,"white")]

showsPsColor (r,g,b) =	showChar ' ' . shows r .
			showChar ' ' . shows g .
			showChar ' ' . shows b .
			showString " scol"

showsFigColor (r,g,b) = showChar ' ' . shows (minPosition 0 (-1,32768*32768)
	[ (x-r)*(x-r) + (y-g)*(y-g) + (z-b)*(z-b) | (x,y,z,_) <- fallBackRgb ])

--
-- find position of minimal element in list
--
minPosition i (pos,min) []       = pos
minPosition i (pos,min) (x:rest) | x < min   = minPosition (i+1) (i,x)     rest
				 | otherwise = minPosition (i+1) (pos,min) rest
