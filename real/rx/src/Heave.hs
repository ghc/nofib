-- uh yeah, another literate programming tool

module Heave

( heave, gheave, stream
, Formatter
) 

where

import Trace


import System
import Stuff (intersperse)
import Char (isSpace)

import Options
import Command

------------------------------------------------------------------



-- we use a stack of options
type Optss = [Opts] 

-------------------------------------------------------

{- 

an essay on line breaking:

inline code (in between $) will be sent directly to the code parser,
which calls myLex which ignores linebreaks completely.

display code (.begin ... .end) will be chopped into lines,
those are sent to the code parser separately,
i. e. each line must be a complete expression.

if an expression doesn't fit on one line, you may use
a continuation line: one that starts with a white space.
such lines will be appended to the most recent line
that started at position 0

-}

glueLines :: [String] -> (String, [String])
-- returns first logical line (including continuations), and rest
glueLines [] = ([],[])
glueLines (l : ls) = 
    let (as, bs) = span (\ l' -> null l' || isSpace (head l')) ls
    in	(unlines (l : as), bs)

---------------------------------------------------------

type Formatter a = (Opts, a) -> String -> IO a

-- a formatter reads a line and outputs something (to stdout)
-- it has a state :: a that is chained through
-- it may also read (not change) the environment provided by the unlit-ter

---------------------------------------------------------

unlit :: Formatter a -> (Optss, a) -> [String] -> IO (Optss, a)

unlit f oss [] = return oss

-- commands must start at the beginning of a line
-- and they start with a dot
unlit f oss (('.' : cmd) : rest) =
    do	{ oss' <- unlitcmd f oss cmd
	; unlit f oss' rest
	}

-- otherwise it's not a command
unlit f oss @ (os @ (opts:_), state) lines =
    caseopts opts "current"
	[ ("text", do 	{ let (h : t) = lines 
			; unlittext f oss h
			; maybePutChar opts '\n'
			; unlit f oss t
			} )
	, ("code", do	{ let (h, t) = glueLines lines
		
			-- start of line hook
			; if onoff opts "output"
			  then	caseopts opts "code"
				[ ("latex", putStr "\\\\\n & & ") 
				, ("plain", return ())
				]
			  else return ()

			; oss' <- unlitcode f oss h

			-- end of line hook
			; if onoff opts "output"
			  then	caseopts opts "code"
				[ ("latex", putChar '\n') 
				, ("plain", putChar '\n')
				]
			  else return ()

			; unlit f oss' t
			} )
	]

--------------------------------------------------------------------

unlittext :: Formatter a -> (Optss, a) -> String -> IO ()

-- inline code, look for $..$ (keepsep) or |..| (omitsep)
-- result () because it may not change opts or env

unlittext f oss @ (os @ (opts : _), state) cs =
    do	{ let [keep] = getopt opts "keepsep"
	; let [omit] = getopt opts "omitsep"

	; let (as, bs) = span (\ c -> c /= keep && c /= omit) cs

	; maybePutStr opts as

	; let sep = head bs -- only called when bs /= []
	; let ds = drop 1 bs 
	; let (es, fs) = span (/= sep) ds 
	; let gs = drop 1 fs 

	; let opts1 = addListToOpts opts 
		[("current","code"), ("context", "inline")]

	; if not (null bs) then do
		{ if onoff opts "output" && sep == keep
		  then  if getopt opts "text" == "latex"
			  && getopt opts "code" == "plain"
			then putStr "\\verb;"
			else putChar sep 
		  else return ()

		; f (opts1, state) es

		; if onoff opts "output" && sep == keep
		  then  if getopt opts "text" == "latex"
			  && getopt opts "code" == "plain"
			then putStr ";"
			else putChar sep 
		  else return ()

		; unlittext f oss gs
		}
	  else return ()	-- line finished
	}

--------------------------------------------------------------------

unlitcode :: Formatter a -> (Optss, a) -> String -> IO (Optss, a)
unlitcode f (oss @ (opts: _), state) s =
    do	{ state' <- f (opts, state) s	-- execute code
	; return (oss, state')		
	}


--------------------------------------------------------------------


-- perhaps start or end a code block
-- this: current options, that: previous options
block this that = 
	if -- we've changed current mode
	  getopt this "current" /= getopt that "current" 
	  -- we're latexing
	  && caseopts that "text" [("latex",True),("plain",False)]

	then
	    if	-- we are in code mode now
		caseopts this "current" [("code",True),("text",False)]
		-- we were in text mode before
		&& caseopts that "current" [("text",True),("code",False)]
	  	-- we _are_ printing
	  	&& onoff this "output" 
	    then
	        caseopts this "code"
		    [ ("plain", putStrLn "\\begin{verbatim}")

-- nice hack -----------------------------------------------------------
-- output "%%" at end of line
-- so that latex ignores the "\\"
-- that will be output before first code line
		    , ("latex", putStr "\\begin{eqnarray*} %% hack: ")
-- end hack -------------------------------------------------------------

		    ]
	    else if -- we were in code mode 
	  	caseopts that "current" [("code",True),("text",False)]
	  	-- we return to text mode
	  	&& caseopts this "current" [("text",True),("code",False)]
	  	-- we _were_ printing
	  	&& onoff that "output" 
	    then
	    	caseopts this "code"
		    [ ("plain", putStrLn "\\end{verbatim}")
		    , ("latex", putStrLn "\\end{eqnarray*}")
		    ]	
	    else return ()
	else return ()


unlitcmd :: Formatter a -> (Optss, a) -> String -> IO (Optss, a)
unlitcmd f oss @ (os @ (opts:ros), state ) cmd =
    case pcmd opts cmd of

	-- import a file, change environment locally only
	-- but globally thread the state through
	Import g name -> 
	    do	{ let opts1 = plusOpts opts g
		; block opts1 opts

		; cs <- if name == "-" -- means stdin
			then 
				trace ("\nreading stdin\n") $
				getContents
			else 
				trace ("\nreading file " ++ name ++ "\n") $
				readFile name

		; (opts2, state2) <- unlit f (opts1 : os, state) (lines cs)
		; block opts (head opts2)
		; return (os, state2)
		}

	-- change environment, continue parsing
	Set g -> 
	    do	{ let opts1 = plusOpts opts g
		; block opts1 opts
		; return (opts1 : ros , state)
		}

	-- begin of a display code group
	Begin g -> 
	    do	{ let opts1 = plusOpts opts g
		; block opts1 opts
		; return (opts1 : os, state)
		}

	-- end of a display code group
	End ->
	    if null ros then error "error: extraneous .end"
	    else do
		{ let opts1 = head ros
		; block opts1 opts	-- note: the other way round
		; return (ros, state)
		}

	-- some unknown command
	Unknown cs ->
	    do 	{ putStrLn ("unkown cmd: " ++ cs) 
		; return oss
		}


----------------------------------------------------------------------

-- the command line is preprended to the input
stream opts f init argv = 
    let	
	process arg = 
	    if '=' `elem` arg 
	    then -- it's a binding
		".set (" ++ arg ++ ")"
	    else -- it's a file name
		".import \"" ++ arg ++ "\""

	limbo = [ process arg | arg <- argv ]

    in	unlit f ([opts], init) limbo >> return ()

-- what we do when interpreted (i. e. we type the command line)
-- read arguments from string, read input from file
gheave opts f init args = 
	stream opts f init (words args)


-- what we call when compiled
-- read arguments (don't read stdin per default. give "-" argument instead)
heave opts f init =
	getArgs >>= \ argv ->
	stream opts f init argv 


