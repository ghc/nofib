-- the read-eval-print loop 
-- without the eval


module Heuristic

( heu
)

where

import Options

import Ids

import Syntax

----------------------------------------------------------------------

-- check through a list of operations,
-- prepend these to the input
heuristic :: Opts -> [ String ] -> Exp -> Exp
heuristic opts hs inp = foldr (\ name inp' -> case onoff opts name of
		True -> App (userfun 1 name) [inp']
		False -> inp' 
	) inp hs

-- look through the complete tree and insert heuristics after each "="
heureq :: Opts -> [ String ] -> Exp -> Exp
heureq opts hs (App id args) | idname id == "=" =
	let a : as = args 
	in App id (a : map (heuristic opts hs . heureq opts hs) as)
heureq opts hs (App id args) =
	App id (map (heureq opts hs) args)
heureq opts hs x = x	-- don't change

-- insert heuristics at each top-level expression that is not an assignment
heutop :: Opts -> [ String ] -> Exp -> Exp
heutop opts hs (App id args) | idname id == ";" =
	App id (map (heutop opts hs) args)
heutop opts hs x @ (App id args) | idname id == "=" = x	-- won't change
heutop opts hs x = heuristic opts hs x	-- do change



heu opts hs = heutop opts hs . heureq opts hs