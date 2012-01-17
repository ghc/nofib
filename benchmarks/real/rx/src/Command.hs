-- for parsing formatting commands

-- changes:

-- 25. 4. 97: make '$' refer to value of some binding

module Command

( Cmd(..)
, pcmd
)

where

import Data.Char

import Options

import Lex
import Parse


-- a command starts with a dot (which is eaten before the parser
-- down below is called) and only extends for one line

data Cmd = Begin Opts
	| End
	| Set Opts
	| Import Opts String

	| Unknown String

--------------------------------------------------------

-- parsing commands

paName = litp "Name" (\ cs -> 
	isAlpha (head cs) || isDigit (head cs))


paStrng = litp "String" (\ cs -> head cs == '"') -- rely on the lexer
	`act` \ cs -> drop 1 (take (length cs - 1) cs)

paNameStrng = paName ||! paStrng

paBind opts = 
	(paNameStrng +.. lit "=")
  +.+ 	(   paNameStrng	-- take it literally
	||! (lit "$" ..+ paNameStrng) `act` (getopt opts)
	)
	

paGroup opts = lit "(" ..+ paBind opts `sepBy` lit "," +.. lit ")"
	`act`  listToOpts

paOptGroup opts = paGroup opts ||! succeed emptyOpts

paCommand opts =   
	      ( lit "begin" ..+ paOptGroup opts
		`act` \ g -> Begin g
	) ||! ( lit "end"
		`act` \ _ -> End
	) ||! ( lit "set" ..+ paGroup opts
		`act` \ g -> Set g
	) ||! ( lit "import" ..+ paOptGroup opts +.+ paNameStrng
		`act` \ (g, n) -> Import g n 

	) ||! ( many (litp "unknown" (const True)) 
		`act` \ ws -> Unknown (unwords ws) 
	)

pcmd opts inp = simpleParse (paCommand opts) (myLex (uncomment inp))
