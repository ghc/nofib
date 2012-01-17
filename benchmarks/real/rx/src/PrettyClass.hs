module PrettyClass

( PrettyClass (..)

, ppSepp

, tabstop
, linewidth

, alParens, alBrackets, alBraces
, ppCommas
, emitascii, emitlatex

, ppSep2
)

where

import Options

import Pretty -- hslibs/ghc/src/Pretty.lhs, with modifications

tabstop = 4 :: Int
linewidth = 75 :: Int

------------------------------------------------------------------------


alParens opts p = 
    caseopts opts "code"
	[ ("plain", ppBesides 
		[ ppStr "(", ppNest tabstop p, ppStr ")" ])
	, ("latex", ppBesides 
		[ ppStr "\\left(", ppNest tabstop p, ppStr "\\right)" ])
	]

alBrackets opts p =
    caseopts opts "code"
	[ ("plain", ppBesides 
		[ ppStr "[", ppNest tabstop p, ppStr "]" ])
	, ("latex", ppBesides 
		[ ppStr "\\left[", ppNest tabstop p, ppStr "\\right]" ])
	]

alBraces opts p =
    caseopts opts "code"
	[ ("plain", ppBesides 
		[ ppStr "{", ppNest tabstop p, ppStr "}" ])
	, ("latex", ppBesides 
		[ ppStr "\\left\\{", ppNest tabstop p, ppStr "\\right\\}" ])
	]

--------------------------------------------------------------------------

x `ppSep2` y = ppSep [x, y]

ppSepp :: [ Pretty ] -> Pretty
ppSepp [] = ppNil
ppSepp xs = 
    let l = length xs
	(as, bs) = splitAt (l `div` 2) xs
    in	case xs of
	[x] -> x
	_ -> ppSepp as `ppSep2` ppSepp bs
	

ppInterleave2 :: Pretty -> [Pretty] -> Pretty
ppInterleave2 p [] = ppNil
ppInterleave2 p qs = ppi2 False p qs

ppi2 finis p qs = 
    let l = length qs
	(as, bs) = splitAt (l `div` 2) qs
    in 	case qs of [q] -> if finis then q `ppBeside` p else q
 		   [ ] -> error "ppInterleave2"
		   _   -> ppi2 True p as `ppSep2` ppi2 finis p bs


---------------------------------------------------------------------


class PrettyClass a where
	-- prettyprint
	pp :: Opts -> a -> Pretty

	-- prettyprint, with precedence
	ppp :: Opts -> Int -> a -> Pretty

	-- default methods
	pp opts = ppp opts 0
	ppp opts n = pp opts

optslatex = listToOpts [("code","latex")]
optsplain = listToOpts [("code","plain")]


emitascii  :: PrettyClass a => a -> ShowS
emitascii x cs = ppShow linewidth (pp optsplain x) ++ cs

emitlatex  :: PrettyClass a => a -> ShowS
emitlatex x cs = ppShow linewidth (pp optslatex x) ++ cs


--------------------------------------------------------------------------

instance PrettyClass Int  where pp opts n = ppStr (show n)
instance PrettyClass Char  where pp opts n = ppStr (show n)
instance PrettyClass Float  where pp opts n = ppStr (show n)
instance PrettyClass Bool where pp opts n = ppStr (show n)

ppCommas pps = ppInterleave2 ppComma pps

instance PrettyClass a => PrettyClass [a] where
    pp opts xs = alBrackets opts 
	(ppCommas (map (pp opts) xs))

instance (PrettyClass a, PrettyClass b) => PrettyClass (a, b) where
    pp opts (x, y) = alParens opts 
	(ppCommas [pp opts x, pp opts y])

instance (PrettyClass a, PrettyClass b, PrettyClass c) 
	=> PrettyClass (a, b, c) where
    pp opts (x, y, z) = alParens opts 
	(ppCommas [pp opts x, pp opts y, pp opts z])

