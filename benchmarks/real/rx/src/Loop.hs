-- the read-eval-print loop 
-- without the eval


module Loop

( Val
, expformat
)

where

import Options

import Pretty	-- syslib ghc
import PrettyClass (linewidth)


import Syntax (Exp, pr)
import ExpParse (pline)
import PI
import Ids

import Heuristic

import Heave (Formatter)

-----------------------------------------------------------------------

type Val e = Opts -> e -> Exp -> Either String (Exp, e)
-- evaluates, with some environment

-- expformat :: [String] -> Val e -> Formatter (IdTable, e)
expformat hs val (opts0, (pi0, env0)) inp =
    do	{ let (mx, (opts1, pi1)) = pline (opts0, pi0) inp	-- parse input

	; case mx of
            Nothing -> return (pi1, env0)
	    Just y -> do 

		-- prepend some defaults (if evaluating)
		{ let x = chose opts1 "eval" (heu opts1 hs y) y

		-- possibly echo the input (with defaults?)
    		; if onoff opts1 "output" && onoff opts1 "exp"
		  then (putStr (ppShow linewidth 
				(pr opts1 (chose opts1 "expand" x y) )))
		  else (return ())

		-- evaluate input
		; chose opts1 "eval"
		    ( case val opts1 env0 x of
		  Left msg -> 
		    do 	{ maybePutStrLn opts1 msg
			-- continue with old environment
			; return (pi1, env0) 
			}

		  Right (y, env1) ->
		    do	{ if onoff opts1 "output" && onoff opts1 "res"
			  then (putChar '\n' >> putStr (ppShow linewidth 
					(ppSep [ppStr "==", pr opts1 y])))
			  else (return () )

			-- continue with new env
			; return (pi1, env1)
			}
		     )

		(return (pi1, env0))	-- nothing happens

	    }
	}

------------------------------------------------------------------------


