--------------------------------------------------------------------------------
-- Copyright 1994 by Peter Thiemann
-- $Log: FigOutput.hs,v $
-- Revision 1.1  1996/01/08 20:02:34  partain
-- Initial revision
--
-- Revision 1.2  1994/03/15  15:34:53  thiemann
-- added full color support, XColorDB based
--
-- Revision 1.1  1993/08/31  12:31:32  thiemann
-- Initial revision
--
-- $Locker:  $
--------------------------------------------------------------------------------

module FigOutput (figShowsWrapper) where

import Fonts (FONT, fontName, fontScale)
import Color
import Info

--------------------------------------------------------------------------------
figShowsWrapper :: WrapperType
figShowsWrapper title
	 (borderDistX, borderDistY, lineWidth, fatLineWidth, arrowSize, ntFont, tFont, _)
		container@(rx, ry, width, height, inOutY, gobj) =
	showString "#FIG 2.1\n" .
	showString "2 80\n" .
{-	showString "1 80\n" . (origin in lower left) is ignored -}
	figShowsContainer rx height container
  where
  figShowsContainer ax ay (rx, ry, width, height, inOutY, gobj) =
	case gobj of
	AString color font theString ->
		showString "4 0" .			    -- object type, sub_type (left just)
		showsTrueNum (figFont (fontName font)) .    -- font (enumeration type)
		showsTrueNum (fontScale font) .		    -- font_size (points)
		showString " 0"	.			    -- pen
	        showsFigColor color .			    -- color
                showString " 0 0.00000 4" .		    -- depth, angle, font_flags
		showsFigNum height .			    -- height
		showsFigNum width .			    -- length
		showsFigNum ax' .			    -- x
		showsFigNum ay' .			    -- y
		showString (' ':theString++"\1\n")	    -- string
	ABox color rounded content ->
		figShowsContainer ax' ay' content .
		showString "2" .
		showString (if rounded then " 4" else " 2") .
		showString " 0 " .			    -- object, subobject (box), line style
		showsFigNum fatLineWidth .		    -- thickness (pixels)
	        showsFigColor color .			    -- color
		showString " 0 0 0" .			    -- depth, pen, area_fill
		showString " 0.000" .			    -- style_val
		(if rounded then showsFigNum (min width height `div` 2)
		else showString " 0") .
		showString " 0 0\n" .			    -- forward_arrow, backward_arrow
		showsFigPoint ax' ay' .
		showsFigPoint (ax'+width) ay' .
		showsFigPoint (ax'+width) (ay'-height) .
		showsFigPoint ax' (ay'-height) .
		showsFigPoint ax' ay' .
		showsFigLastPoint
	Arrow color size ->
		showString "2 1 0" .			    -- a polyline
		showsFigNum lineWidth .
                showsFigColor color .
		showString " 0 0 0 0.000 -1 1 0\n" .
		showString "        0 0" .		    -- arrow_type, arrow_style
		showsFigNum lineWidth . showString ".000" . -- arrow_thickness
		showsFigNum (abs size * 2) . showString ".000" . -- arrow_width
		showsFigNum (abs size * 2) .showString ".000\n" . -- arrow_height
		showString "        " .
		showsFigPoint (ax'-size) ay' .
		showsFigPoint ax' ay' .
		showsFigLastPoint
	Aline color ->
		showString "2 1 0" .
		showsFigNum lineWidth .
	        showsFigColor color .
		showString " 0 0 0 0.000 -1 0 0\n" .
		showString "        " .
		showsFigPoint ax' ay' .
		showsFigPoint (ax'+width) (ay'-height) .
		showsFigLastPoint
	ATurn color dir ->
		showString "3 0 0" .			    -- a spline object
		showsFigNum lineWidth .
	        showsFigColor color .
		showString " 0 -1 0 0.0 0 0\n" .
		showsIt dir .
		showsFigLastPoint
		where	showsIt SE =	showsFigPoint ax' ay' .
					showsFigPoint ax' (ay'-height) .
					showsFigPoint (ax'+width) (ay'-height)
			showsIt WN =	showsFigPoint ax' ay' .
					showsFigPoint (ax'+width) ay' .
					showsFigPoint (ax'+width) (ay'-height)
			showsIt SW =	showsFigPoint (ax'+width) ay' .
					showsFigPoint (ax'+width) (ay'-height) .
					showsFigPoint ax' (ay'-height)
			showsIt NE =	showsFigPoint ax' (ay'-height) .
					showsFigPoint ax' ay' .
					showsFigPoint (ax'+width) ay'
	AComposite contents ->
		showString "6" .
		showsFigPoint (ax'+width) (ay'-height) .
		showsFigPoint ax' ay' .
		showChar '\n' .
		foldr (.) (showString "-6\n") (map (figShowsContainer ax' ay') contents)
    where	ax' = ax + rx
		ay' = ay - ry

figFont name = lookup figFontList 0
    where
        lookup [] _ = -1
	lookup (font: fonts) n | font == name = n
			       | otherwise    = lookup fonts (n+1)

figFontList = [						    -- stolen from u_fonts.c
	"Times-Roman",
	"Times-Italic",
	"Times-Bold",
	"Times-BoldItalic",
	"AvantGarde-Book",
	"AvantGarde-BookOblique",
	"AvantGarde-Demi",
	"AvantGarde-DemiOblique",
	"Bookman-Light",
	"Bookman-LightItalic",
	"Bookman-Demi",
	"Bookman-DemiItalic",
	"Courier",
	"Courier-Oblique",
	"Courier-Bold",
	"Courier-BoldOblique",
	"Helvetica",
	"Helvetica-Oblique",
	"Helvetica-Bold",
	"Helvetica-BoldOblique",
	"Helvetica-Narrow",
	"Helvetica-Narrow-Oblique",
	"Helvetica-Narrow-Bold",
	"Helvetica-Narrow-BoldOblique",
	"NewCenturySchlbk-Roman",
	"NewCenturySchlbk-Italic",
	"NewCenturySchlbk-Bold",
	"NewCenturySchlbk-BoldItalic",
	"Palatino-Roman",
	"Palatino-Italic",
	"Palatino-Bold",
	"Palatino-BoldItalic",
	"Symbol",
	"ZapfChancery-MediumItalic",
	"ZapfDingbats"]

showsTrueNum :: Int -> ShowS
showsTrueNum x = showChar ' ' . shows x

showsFigNum :: Int -> ShowS
showsFigNum x = showChar ' ' . shows ((x*9 + 999) `div` 1000)	    -- sorry about that

showsFigPoint :: Int -> Int -> ShowS
showsFigPoint x y = showsFigNum x . showsFigNum y

showsFigLastPoint :: ShowS
showsFigLastPoint = showString " 9999 9999\n"

-- showsFigColor :: Int -> ShowS
-- showsFigColor c = showChar ' ' . showsColor c
