--                            -*- Mode: Haskell -*- 
-- Copyright 1994 by Peter Thiemann
-- Ebnf2ps.hs --- the driver module for the syntax diagram generator
-- Author          : Peter Thiemann
-- Created On      : Fri Aug 27 09:09:15 1993
-- Last Modified By: Peter Thiemann
-- Last Modified On: Mon Dec 27 17:41:11 1993
-- Update Count    : 28
-- Status          : Unknown, Use with caution!
-- 
-- $Log: Main.hs,v $
-- Revision 1.5  1997/03/19 01:03:38  simonpj
-- Fix nofib/real/ebnf2
--
-- Revision 1.4  1997/03/17 20:35:26  simonpj
-- More small changes towards 2.02
--
-- Revision 1.3  1997/03/14 08:08:10  simonpj
-- Major update to more-or-less 2.02
--
-- Revision 1.2  1996/07/25 21:24:02  partain
-- Bulk of final changes for 2.01
--
-- Revision 1.1  1996/01/08 20:02:39  partain
-- Initial revision
--
-- Revision 1.3  1994/03/15  15:34:53  thiemann
-- added full color support, XColorDB based
--
-- Revision 1.2  1993/09/13  10:37:39  thiemann
-- Fixed a space leak
--
-- Revision 1.1  1993/08/31  12:31:32  thiemann
-- Initial revision
--
-- $Locker:  $
--

module Main (main)
where

import System.IO
import IOSupplement
import CommandLine	(parse_cmds)
import StringMatch	(stringMatch)
import Fonts		(FONT, makeFont)
import EbnfGrammar
import HappyParser	(theHappyParser)
import AbstractSyntax
import GrammarTransform (simplify)
import EbnfLayout
import FigOutput	(figShowsWrapper)
import PsOutput		(psShowsWrapper)
import Color
import Info

--------------------------------------------------------------------------------
main =  parse_cmds program
--------------------------------------------------------------------------------
program   
	  :: String -> Int -> String
	  -> String -> Int -> String
	  -> String -> String
	  -> Int -> Int
	  -> Int -> Int
	  -> Int					    -- arrowSize
	  -> String					    -- rgbFileName
	  -> Bool
	  -> Bool
	  -> Bool -> Bool
	  -> Bool -> Bool
	  -> [String] -> IO ()
program
	  ntFontName ntFontScale ntColor
	  tFontName  tFontScale  tColor
	  lineColor fatLineColor
	  borderDistX borderDistY
	  lineWidth fatLineWidth
	  arrowSize 
	  rgbFileName
	  happyInput
	  doSimplify
	  psOutput figOutput
	  helpFlag verbose strs
	  | length strs < 2 || helpFlag	=
	      hPutStr stderr
	      		("Usage: ebnf2ps [options] BNFfile Nonterminal ...\n"
	      		++unlines usageBlurb)
	  | otherwise =
	    do
	      afmPath <- getPath "AFMPATH" afmPathDefault
	      ntAFM <- readPathFile afmPath (ntFontName++".afm")
	      tAFM <- readPathFile afmPath (tFontName++".afm")
	      rgbPath <- getPath "RGBPATH" rgbPathDefault
	      rgbFileContents <- readPathFile rgbPath rgbFileName
				 `catch`
				 (\ _ -> 
				  do
				    message "Color database not found, using fall back data\n"
				    return "")

	      let 
		  colorTable = prepareColors rgbFileContents
		                     [ntColor, tColor, lineColor, fatLineColor]
		  colorInfo@(c1,c2,c3,c4) = (lookupColor ntColor colorTable,
		                   lookupColor tColor colorTable,
                                   lookupColor lineColor colorTable,
		                   lookupColor fatLineColor colorTable)
		  info = (borderDistX, borderDistY, lineWidth, fatLineWidth, arrowSize,
		          makeFont ntFontName ntFontScale ntAFM,
		          makeFont tFontName tFontScale tAFM,
		  	  colorInfo)

	      message ("using colors: "++(showsColor c1 . showsColor c2 .
		                          showsColor c3 . showsColor c4)
		      "\nfrom rgbPathDefault: "++show rgbPathDefault)

	      inputPath <- getPath "EBNFINPUTS" ebnfInputDefault

  	      message ("generating nonterminals: "++show nonterminals++
		      "\nfrom "++bnfName++
		      "\nusing input path "++show inputPath)

	      bnfContent <- readPathFile inputPath bnfName

	      if happyInput then
		let rawInput = theHappyParser bnfContent
	  	    prods | doSimplify = simplify rawInput
			  | otherwise  = rawInput
		in do
			message "using happyInput"
		        writeAll outExtension (layoutAll outWrapper info prods nonterminals)
	        else
	        case map (if doSimplify then simplify else id) (parseAll bnfContent) of
		    prods:_ -> do 
				  message "using ebnfInput"
				  writeAll outExtension (layoutAll outWrapper info prods nonterminals)
		    _ -> hPutStr stderr ("Could not parse "++bnfName++"\n")
		
    where
	afmPathDefault      = ["/usr/local/tex/Adobe", "/usr/local/tex/lib/TeXPS/afm", "."]
	ebnfInputDefault    = ["."]
	rgbPathDefault	    = ["/usr/lib/X11", "/usr/local/X11R5/lib/X11"]
	(bnfName: nonterminals) = strs
	(outWrapper, outExtension)
		| figOutput = (figShowsWrapper, ".fig")
		| otherwise = (psShowsWrapper,  ".eps")
	message what | verbose   = hPutStr stderr (what++"\n")
	             | otherwise = return ()

--------------------------------------------------------------------------------
layoutAll :: WrapperType -> INFO -> [Production] -> [String] -> [(String, String)]
layoutAll wrapper info prods nonterminals =
	[ (ntName, wrapper ntName info (makePictureLayout info prod) "")
	| prod@(ProdProduction ntName ntAliases _) <- prods,
--	  ntName `elem` nonterminals ]
	  any (flip stringMatch ntName) nonterminals ]

--------------------------------------------------------------------------------
usageBlurb =
	[ "",
	  "where options may be chosen from the following list:",
	  "",
          "  -ntFont        <font>\tPostScript font used for nonterminals",
	  "  -ntScale        <int>\tpointsize of typeface for nonterminals",
	  "  -ntColor      <color>\tcolor of typeface for nonterminals",
	  "  -tFont         <font>\tPostScript font used for terminal strings",
	  "  -tScale         <int>\tpointsize of typeface for terminals",
	  "  -tColor       <color>\tcolor of typeface for terminals",
	  "  -borderDistX    <int>\thorizontal distance of objects from their container",
	  "  -borderDistY    <int>\tvertical distance of objects from their container",
	  "  -lineWidth      <int>\tused for connecting lines",
	  "  -fatLineWidth   <int>\tused for boxes",
	  "  -lineColor    <color>\tcolor used for connecting lines",
	  "  -fatLineColor <color>\tused for boxes",
	  "  -arrowSize      <int>\tsize of (invisible) box containing an arrow",
	  "  -rgbFileName    <int>\tfile name for color definitions (default \"rgb.txt\")",
	  "  -happy               \taccept happy input format",
	  "  +ps                  \tproduce encapulated PostScript output (default)",
	  "  +fig                 \tproduce fig output (FORMAT 2.1)",
	  "  +simplify            \tsimplify productions (experimental)",
	  "  -verbose             \tprint some progress messages",
	  "  -help                \tproduces this list",
	  "",
	  "Only the first occurrence of an option is recognized.",
	  "Environment variables:",
	  "",
	  "  AFMPATH\tsearch path for Adobe Font Metric files",
	  "  EBNFINPUTS\tsearch path for BNFfiles",
	  "  RGBPATH\tsearch path for color definitions"
	]

--------------------------------------------------------------------------------

writeAll ext [] = return ()
writeAll ext ((ntName, content): more) 
  = do
	hPutStr stdout content
	writeAll ext more

--------------------------------------------------------------------------------

str2int :: String -> Int
str2int s = case reads s of
	    []    -> 0
	    ((x,_):_) -> x
