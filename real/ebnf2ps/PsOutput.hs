--------------------------------------------------
-- Copyright 1994 by Peter Thiemann
-- $Log: PsOutput.hs,v $
-- Revision 1.1  1996/01/08 20:02:34  partain
-- Initial revision
--
-- Revision 1.4  1994/03/15  15:34:53  thiemann
-- added full color support, XColorDB based
--
--Revision 1.3  1993/08/31  12:31:32  thiemann
--Reflect changes in type FONT
--
--Revision 1.2  1993/08/25  15:11:11  thiemann
--added PostScript prolog to use shorter command names
--fixed backslash bug in psString
--
--Revision 1.1  1993/08/17  12:34:29  thiemann
--Initial revision
--
-- $Locker:  $
--------------------------------------------------
module PsOutput (psShowsWrapper) where

-- import EbnfLayout
import Fonts (FONT, fontName, fontScale, noFont)
import Color (Color (..), showsPsColor, noColor)
import Info (Container (..), GObject (..), TDirection (..), WrapperType (..), INFO(..), ColorInfo(..))

-- psState = (currentColor, currentFont, currentLinewidth)
type PsState = (Color, FONT, Int, ShowS)
type PsTrafo = PsState -> PsState

initialState = (noColor, noFont, -1, id)

setColor :: Color -> PsTrafo
setColor clr st@(clr0, fnt0, lw0, shower)
  | clr == clr0 = st
  | otherwise   = (clr, fnt0, lw0, shower . showsPsColor clr)

setFont :: FONT -> PsTrafo
setFont font st@(clr0, fnt0, lw0, shower)
  | font == fnt0 = st
  | otherwise    = (clr0, font, lw0,
		    shower .
                    showString ('/':fontName font) . showString " findfont " .
                    shows (fontScale font) . showString " scalefont" .
		    showString " setfont\n")
		   

setLineWidth :: Int -> PsTrafo
setLineWidth lw st@(clr0, fnt0, lw0, shower)
  | lw == lw0 = st
  | otherwise = (clr0, fnt0, lw, shower . showsPsNum lw . showString " slw\n")

drawBox :: Bool -> Int -> Int -> Int -> Int -> Int -> PsTrafo
drawBox rounded ax ay width height lw (clr0, fnt0, lw0, shower) = 
	 (clr0, fnt0, lw,
	  shower . showsPsNum ax . showsPsNum ay .
	  showsPsNum width . showsPsNum height . showsPsNum lw .
	  showString (if rounded then " RBox\n" else " Box\n"))

drawString :: Int -> Int -> String -> PsTrafo
drawString ax ay str (clr0, fnt0, lw0, shower) = 
	(clr0, fnt0, lw0, 
	 shower .
	 showsMoveto ax ay .
	 showChar '(' . showString (psString str) . showChar ')' .
	 showString " show\n")

drawRLine :: Int -> Int -> [(Int, Int)] -> PsTrafo
drawRLine ax ay rels (clr0, fnt0, lw0, shower) =
	(clr0, fnt0, lw0,
	 shower .
	 showString "n" .
	 showsMoveto ax ay .
	 foldr (.) (showString " s\n") [ showsRLineto rx ry | (rx, ry) <- rels ])

insertShowS :: ShowS -> PsTrafo
insertShowS shower1 (clr0, fnt0, lw0, shower) = (clr0, fnt0, lw0, shower . shower1)

runTrafo :: PsTrafo -> ShowS
runTrafo f = shower where
		      (_, _, _, shower) = f initialState

psShowsWrapper :: WrapperType
psShowsWrapper title
	 (borderDistX, borderDistY, lineWidth, fatLineWidth, arrowSize, ntFont, tFont,
	  (ntColor, tColor, lineColor, fatLineColor))
		container@(rx, ry, width, height, inOutY, gobj) =
	showString "%!PS-Adobe-1.0\n" .
	showString "%%DocumentFonts: " .
	showString ntFontName . 
	(if ntFontName == tFontName then id else (showChar ' ' . showString tFontName)) .
	showString "\n%%Title: " . showString title .
	showString "\n%%Creator: ebnf2ps (Copyright 1994 by Peter Thiemann)\n" .
	showString "%%Pages: 0\n" .
	showString "%%BoundingBox:" . 
	showsPsNum (psFloor rx) . showsPsNum (psFloor ry) .
	showsPsNum (psCeil (rx+width)) . showsPsNum (psCeil (ry+height)) .
	showString "\n%%EndComments\n" .
	showString psProlog .
	showString "%%EndProlog\n" .
	showString "\n$Ebnf2psBegin\n" .
	runTrafo (psShowsContainer rx ry container) .
	showString "\n$Ebnf2psEnd\n"
	where
  ntFontName = fontName ntFont
  tFontName  = fontName tFont

  psShowsContainer :: Int -> Int -> Container -> PsTrafo
  psShowsContainer ax ay (rx, ry, width, height, inOutY, gobj) =
	case gobj of
	AString color font theString ->
		drawString ax1 ay1 theString .
		setColor color .
		setFont font
	ABox color rounded content ->
		psShowsContainer ax1 ay1 content .
		drawBox rounded ax1 ay1 width height fatLineWidth .
		setColor color
        Arrow color size ->
	    drawRLine  (ax1-size) (ay1+abs size) [(size, -abs size), (-size, -abs size)] .
	    setLineWidth lineWidth .
	    setColor color
	Aline color ->
	    drawRLine ax1 ay1 [(width, height)] .
	    setLineWidth lineWidth .
	    setColor color
	ATurn color dir ->
	    insertShowS(
		showString "n" .
		showsIt dir .
		showString " s\n") .
	    setLineWidth lineWidth .
	    setColor color
		where
		  showsIt SE = showsMoveto ax1 ay1 .
			       showsArcto ax1 (ay1+height) (ax1+width) (ay1+height) radius .
			       showsLineto (ax1+width) (ay1+height)
		  showsIt WN = showsMoveto ax1 ay1 .
			       showsArcto (ax1+width) ay1 (ax1+width) (ay1+height) radius .
			       showsLineto (ax1+width) (ay1+height)
		  showsIt SW = showsMoveto (ax1+width) ay1 .
			       showsArcto (ax1+width) (ay1+height) ax1 (ay1+height) radius .
			       showsLineto ax1 (ay1+height)
		  showsIt NE = showsMoveto (ax1+width) ay1 .
			       showsArcto ax1 ay1 ax1 (ay1+height) radius .
			       showsLineto ax1 (ay1+height)
		  radius = min height width
	AComposite contents ->
		foldr (.) id (map (psShowsContainer ax1 ay1) contents)
    where
      ax1 = ax + rx
      ay1 = ay + ry

-- showsPsColor color =	showString " col" . showsColor color

showsSetlinewidth lineWidth = showsPsNum lineWidth . showString " slw"

showsMoveto x y	=	showsPsXY x y . showString " m"

showsLineto x y =	showsPsXY x y . showString " l"

showsArcto x1 y1 x2 y2 r = showsPsXY x1 y1 . showsPsXY x2 y2 . showsPsNum r .
			   showString " apr\n"

showsRMoveto x y =	showsPsXY x y . showString " rm"

showsRLineto x y =	showsPsXY x y . showString " rl"

showsPsXY x y =		showsPsNum x . showsPsNum y

showsPsNum :: Int -> ShowS
showsPsNum x =		showChar ' ' . shows x100 .
			if x99 == 0 then id
			else showChar '.' . shows x1 . shows x2
			where (x100,x99) = x `divMod` 100
			      (x1,x2) = x99 `divMod` 10

psFloor, psCeil :: Int -> Int
psFloor x = 100 * (x `div` 100)
psCeil  x = 100 * ((x + 99) `div` 100)

-- showsPsInt :: Int -> showS
-- showsPsInt x = showChar ' ' . showInt (x `div` 100)
	
psString "" = ""
psString ('(':cs) = "\\("   ++ psString cs
psString (')':cs) = "\\)"   ++ psString cs
psString ('\\':cs)= "\\\\"  ++ psString cs
psString ('-':cs) = "\\261" ++ psString cs		    -- endash looks much nicer
psString (c:cs)   = c:psString cs

-- Box:		width height linewidth Box -> -
-- draw box at current point

psProlog :: String
psProlog = "\
\/$Ebnf2psDict 100 dict def\n\
\$Ebnf2psDict begin\n\
\/l {lineto} bind def\n\
\/m {moveto} bind def\n\
\/rl {rlineto} bind def\n\
\/rm {rmoveto} bind def\n\
\/s {stroke} bind def\n\
\/n {newpath} bind def\n\
\/gs {gsave} bind def\n\
\/gr {grestore} bind def\n\
\/clp {closepath} bind def\n\
\/slw {setlinewidth} bind def\n\
\/graycol {dup dup currentrgbcolor 4 -2 roll mul 4 -2 roll mul\n\
\4 -2 roll mul setrgbcolor} bind def\n\
\/scol {3 {255 div 3 1 roll} repeat setrgbcolor} bind def\n\
\ \
\/apr {arcto 4 {pop} repeat} def\n\
\/Box {\n\
\  /linewidth exch def\n\
\  linewidth sub /height exch def\n\
\  linewidth sub /width exch def\n\
\ \
\  n m\n\
\  width 0 rl\n\
\  0 height rl\n\
\  width neg 0 rl\n\
\  0 height neg rl\n\
\  clp linewidth slw s\n\
\} def\n\
\ \
\/RBox {\n\
\  /linewidth exch def\n\
\  /height exch def\n\
\  /width exch def\n\
\  /lly exch def\n\
\  /llx exch def\n\
\  linewidth 2 div dup llx add /llx exch def lly add /lly exch def\n\
\  /height height linewidth sub def\n\
\  /width  width  linewidth sub def\n\
\  /height2 height 2 div def\n\
\  /width2  width  2 div def\n\
\  /urx llx width add def\n\
\  /ury lly height add def\n\
\  /mmx llx width2 add def\n\
\  /mmy lly height2 add def\n\
\  /radius width2 height2 ge {height2} {width2} ifelse def\n\
\ \
\  n mmx lly m\n\
\  urx lly urx mmy radius apr\n\
\  urx ury mmx ury radius apr\n\
\  llx ury llx mmy radius apr\n\
\  llx lly mmx lly radius apr\n\
\  mmx lly l\n\
\  clp linewidth slw s\n\
\} def\n\
\end\n\
\/$Ebnf2psBegin {$Ebnf2psDict begin /$Ebnf2psEnteredState save def} def\n\
\/$Ebnf2psEnd {$Ebnf2psEnteredState restore end} def\n\
\\n"
