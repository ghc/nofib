--------------------------------------------------------------------------------
-- Copyright 1994 by Peter Thiemann
-- $Log: Fonts.hs,v $
-- Revision 1.7  2000/01/24 17:14:26  simonmar
-- Undo fromInt changes: already converted to fromIntegral.
--
-- Revision 1.6  1999/12/08 09:56:37  simonmar
-- -syslib updates for new libraries.
--
-- Revision 1.5  1999/11/26 10:29:54  simonpj
-- fromInt wibble
--
-- Revision 1.4  1999/09/14 10:18:24  simonmar
-- Replace all instances of fromInt in nofib with fromIntegral.
--
-- We generate the same code in most cases :-)
--
-- Revision 1.3  1997/03/14 08:08:05  simonpj
-- Major update to more-or-less 2.02
--
-- Revision 1.2  1996/07/25 21:23:54  partain
-- Bulk of final changes for 2.01
--
-- Revision 1.1  1996/01/08 20:02:33  partain
-- Initial revision
--
-- Revision 1.1  1993/08/31  12:31:32  thiemann
-- Initial revision
--
-- Revision 1.1  1993/08/31  12:31:32  thiemann
-- Initial revision
--
-- $Locker:  $
--------------------------------------------------------------------------------

module Fonts (FONT, makeFont, fontDescender, stringWidth, stringHeight, fontName, fontScale, noFont)
where

import Data.Char

-- not in 1.3
readDec :: (Integral a) => ReadS a
readDec = readInt 10 isDigit (\d -> ord d - ord_0)

readInt :: (Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s =
    [(foldl1 (\n d -> n * radix + d) (map (fromIntegral . digToInt) ds), r)
	| (ds,r) <- nonnull isDig s ]

ord_0 :: Num a => a
ord_0 = fromIntegral (ord '0')

nonnull                 :: (Char -> Bool) -> ReadS String
nonnull p s             =  [(cs,t) | (cs@(_:_),t) <- [span p s]]

readSigned :: (Real a) => ReadS a -> ReadS a
readSigned readPos = readParen False read'
		     where read' r  = read'' r ++
				      [(-x,t) | ("-",s) <- lex r,
						(x,t)   <- read'' s]
			   read'' r = [(n,s)  | (str,s) <- lex r,
		      				(n,"")  <- readPos str]



data FONT = FONT String Int Int (String -> Int)

instance Eq FONT where
  FONT s1 m1 n1 f1 == FONT s2 m2 n2 f2 = s1 == s2 && m1 == m2 && n1 == n2

noFont = FONT "" 0 0 (const 0)

data Afm = Descender Int
	 | CharMetric Int    Int    String   Int Int Int Int
--	   CharMetric charNo charWX charName llx lly urx ury
--	 deriving Text

fontName :: FONT -> String
fontName (FONT name _ _ _) = name

fontScale :: FONT -> Int
fontScale (FONT _ scale _ _) = scale

fontDescender :: FONT -> Int
fontDescender (FONT _ _ theDescender _) = theDescender

stringWidth :: FONT -> String -> Int
stringWidth (FONT _ _ _ theStringWidth) = theStringWidth

stringHeight :: FONT -> String -> Int
stringHeight (FONT _ scale _ _) _ = scale * 100

makeFont :: String -> Int -> String -> FONT
makeFont fontName fontScale fontAfm =
	FONT fontName fontScale theDescender
	((`div` 10). (* fontScale). getStringWidth parsedAfm)
    where
	parsedAfm = parseAfmFile (lines fontAfm)
	theDescender = getDescender parsedAfm

getStringWidth :: [Afm] -> String -> Int
getStringWidth afms str = sum (map (getCharWidth afms . fromEnum) str)

getCharWidth :: [Afm] -> Int -> Int
getCharWidth (CharMetric charNo charWX charName llx lly urx ury: afms) chNo
	| charNo == chNo = charWX
	| otherwise      = getCharWidth afms chNo
getCharWidth (_:afms) chNo = getCharWidth afms chNo
getCharWidth [] chNo = 0

getDescender :: [Afm] -> Int
getDescender (Descender d: _) = d
getDescender (_:rest) = getDescender rest
getDescender [] = 0

--------------------------------------------------------------------------------

parseAfmFile :: [String] -> [Afm]
parseAfmFile [] = []
parseAfmFile (('D':'e':'s':'c':'e':'n':'d':'e':'r':line):lines) =
	Descender descender: parseAfmFile lines
	where (descender,_):_ = readSigned readDec (skipWhite line)
parseAfmFile (('E':'n':'d':'C':'h':'a':'r':'M':'e':'t':'r':'i':'c':'s':_):_) = []
parseAfmFile (('C':' ':line):lines) = CharMetric charNo charWX charName llx lly urx ury:
				  parseAfmFile lines
	where	(charNo, rest1):_ = readSigned readDec (skipWhite line)
		'W':'X':rest2	  = skipWhiteOrSemi rest1
		(charWX, rest3):_ = readDec (skipWhite rest2)
		'N':rest4	  = skipWhiteOrSemi rest3
		(charName, rest5) = span isAlpha (skipWhite rest4)
		'B':rest6	  = skipWhiteOrSemi rest5
		(llx, rest7):_	  = readSigned readDec (skipWhite rest6)
		(lly, rest8):_	  = readSigned readDec (skipWhite rest7)
		(urx, rest9):_	  = readSigned readDec (skipWhite rest8)
		(ury, _):_	  = readSigned readDec (skipWhite rest9)
parseAfmFile (_:lines) = parseAfmFile lines

skipWhite = dropWhile isSpace
skipWhiteOrSemi = dropWhile isSkipChar
isSkipChar c = isSpace c || c == ';'

		
