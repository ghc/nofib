module Reader

( rg 	-- reads a grammar from a string
	-- returns corresponding deterministic automaton
)

where

import ExpParse (pline)

import FiniteMap

import Options
import Defaults

import Ids
import IdStack
import Gen

import FA
import FAtypes
import Gram2FA

import Syntax
import Semantik

--------------------------------------------------------------------

rg :: String -> BDFA Int
rg cs = 
    let
	(Just x, _) = pline (opts0, genpid) cs
	g = docomp opts0 genenv x
		
    in
	t2d opts0 g


