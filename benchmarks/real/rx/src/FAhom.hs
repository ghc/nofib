module FAhom

( homBDFA
, homTNFA
)

where

import Set
import FiniteMap

import Options

import Stuff

import TA
import FAtypes




homBDFA :: (Ord a, Ord b) => Opts -> (a -> b) -> (BDFA a -> BDFA b)
-- homomorphism: identifies some states
homBDFA opts f (BDFA cons all starts moves) =
    let 
	-- some paranoid checks first
	nostarts = all `minusSet` starts
	starts' = mapSet f starts
	nostarts' = mapSet f nostarts
	all' = starts' `unionSet` nostarts'

	moves' = addListToFM_C 
		(\ x y -> if x /= y 
			then error "bfdahom identifies incosistent ruleset"
			else x)
		emptyFM
		[ (mksterm (stcon t) (map f (stargs t)), f w)
		| (t, w) <- fmToList moves
		]
		
    in	if not (isEmptySet (starts' `intersectSet` nostarts'))
	then error "homBDFA identifies starts and nostarts"
	else	BDFA cons all' starts' moves'

---------------------------------------------------------------

homTNFA :: (Ord a, Ord b) => Opts -> (a -> b) -> (TNFA a -> TNFA b)
-- homomorphism: identifies some states
homTNFA opts f (TNFA cons all starts moves) =
    let 
	-- can't do paranoia checking here
	-- since rejecting states are not uniquely determined

	starts' = mapSet f starts
	all' = mapSet f all

	moves' = addListToFM_C 
		(\ x y -> if x /= y 
			then error "bfdahom identifies incosistent ruleset"
			else x)
		emptyFM
		[ ( f w 
		  , mapSet ( \ t -> mksterm (stcon t) (map f (stargs t))) ts )
		| (w, ts) <- fmToList moves
		]
		
    	g = TNFA cons all' starts' moves'

    in	
	trinfo opts "hom" g $

	g
------------------------------------------------------------------------
