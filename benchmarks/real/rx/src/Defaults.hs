module Defaults 

( opts0
)

where

import Options

opts0 :: Opts

-- default options for simple pocket calculator use

opts0 = listToOpts
	[ ("version", "0.0.0")

	-- formatting
	, ("output", "on") 
	, ("text", "plain")
	, ("code", "plain")
	, ("current", "code")

	-- parsing
	, ("keepsep", "$")
	, ("omitsep", "|")

	, ("implicit",  "on")	-- do CL parsing
	, ("apply", "@")	-- use that as apply symbol

	-- input preprocessing (do it, but don't show it)
	, ("expand", "off")
	, ("unify", "on")
	, ("useful", "on")
	, ("min", "on")		-- this implies det
	, ("det", "off")	-- therefore det is off


	-- output postprocessing (do it)
	, ("foldconst", "on")
	, ("foldnonrec", "on")
	, ("hidegrammar", "on")

	-- course of evaluation
	, ("exp", "on")		-- echo input
	, ("eval", "on")	-- evaluate it
	, ("trace", "off")	
	, ("res", "on")		-- show result

	, ("reuse", "off")	-- tricks with reductions?

	]