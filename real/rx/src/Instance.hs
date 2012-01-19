module Instance 

( instpublic
)

-- find some instances of a language
-- just delete all recursive rules in the automaton

where

import Trace

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes




weed moves current seen =
    if isEmptySet current then moves
    else 
	let
	    -- reachables
	    neigh = mkSet [ d
			| c <- setToList current
			, st <- setToList (lookupset moves c)
			, d <- stargs st
			]

	    seen' =  (seen `unionSet` current)
	    current' =  (neigh `minusSet` seen) 

	    moves' = mapFM (\ q sts -> mkSet [ st  | st <- setToList sts
					, q `elementOf` seen
					  || not (or [ p `elementOf` seen' 
					      | p <- stargs st ])
					]
		) moves
	in
	    weed moves' current' seen'


inst :: TNFA Int -> Set Int -> TNFA Int
inst a @ (TNFA cons all starts moves) is =
    let
	moves' = weed moves is emptySet
    in  TNFA cons all is moves'

instpublic :: Opts -> [ TNFA Int ] -> TNFA Int

instpublic opts args =
    if length args /= 1 
    then error "instpublic.args"
    else 
	let [ arg1 @ (TNFA cons all starts moves) ] = args
	in  inst arg1 starts


