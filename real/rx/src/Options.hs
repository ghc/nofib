module Options

( Opts

, emptyOpts
, listToOpts
, addToOpts
, addListToOpts
, plusOpts

, getopt
, caseopts
, onoff
, chose

, troff

, maybeIO
, maybePutStr
, maybePutStrLn
, maybePutChar

)

where

import Trace

import Stuff (intersperse)

import FiniteMap -- from syslib ghc

type Opts = FiniteMap String String

emptyOpts :: Opts
emptyOpts = emptyFM

listToOpts :: [(String, String)] -> Opts
listToOpts = listToFM

addListToOpts :: Opts -> [(String, String)] -> Opts
addListToOpts = addListToFM

addToOpts :: Opts -> String -> String -> Opts
addToOpts = addToFM

plusOpts :: Opts -> Opts -> Opts
plusOpts = plusFM

getopt :: Opts -> String -> String
getopt opts name =
    lookupWithDefaultFM opts
	(error ("no argument for option: " ++ name))
	name

caseopts :: Opts -> String -> [(String, a)] -> a
caseopts opts name acts =
    let val = lookupWithDefaultFM opts (wrong Nothing) name


	quote s = "`" ++ s ++ "'"
	wrong v = error (unlines 
	    [ "error: when looking up option " ++ quote name
	    , case v of 
		Nothing -> "error: value not specified"
		Just val -> "error: value " ++ quote val ++ " not understood"
	    , "error: possible values are: " 
		++ concat (intersperse ", " (map (quote . fst) acts))
	    , "error: program stops"
	    ] )
		
    in	case lookup val acts of
	Just act -> act
	Nothing -> wrong (Just val)


onoff :: Opts -> String -> Bool
onoff opts name = caseopts opts name [("on", True),("off",False)]

chose :: Opts -> String -> a -> a -> a
chose opts name yeah noh = if onoff opts name then yeah else noh

maybeIO opts io =
    case onoff opts "output" of 
	True -> io
	False -> return ()

maybePutStr   opts s =  maybeIO opts (putStr   s)
maybePutStrLn opts s =  maybeIO opts (putStrLn s)
maybePutChar  opts c =  maybeIO opts (putChar  c)

troff :: Opts -> String -> a -> a
troff opts msg = chose opts "trace" (trace msg) id

