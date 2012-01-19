module WrapSubtrans

where

import FAtypes
import FAsubtrans

import Syntax
import Ids
import Semantik
import Options

import FA2Exp

subtrans :: Opts -> Env Auto Auto -> [Exp] -> FIO (Auto, Env Auto Auto)
subtrans opts env args =
    do	{ moops (length args /= 2)
		( "subtrans needs exactly 2 arguments" ++ show args )
	; let [f, a] = args
	; v <- case f of 
	    App id _ -> return id
	    _ -> oops ("first arg of subtrans must be function name " 
			++ show args)
	; (w, _) <- comp opts env a

	-- for converting auto to exp
	; let opts1 = addListToOpts opts
		[("expand","off"),("foldconst","off"),("foldnonrec","off")]

	-- for computing intermediate results
	; let opts2 = addListToOpts opts
		[("min","off"),("det","off")]

	; return ( subtransTNFA opts 
			(\ opts a -> docomp opts2 env 
				(App v [tnfa2exp opts1 a]))
			w
		, env )
	}
