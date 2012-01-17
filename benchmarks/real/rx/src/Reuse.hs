module Reuse

( reuse

)

where

import Trace

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids


import FAmap
import FAkeepst

reuse :: Opts -> TNFA Int -> [Int] -> TNFA Int


-- replace some of the states used in |news|
-- by eps moves to states already used in |moves|

reuse1 opts a @ (TNFA cons all starts moves) addons =

    let
	rmoves = invert moves

	-- find possible covers for addon state b

	-- relies on strange behaviour of 
	-- intersectManySets [] = emptySet
	cands b = intersectManySets
		[ lookupset rmoves t `minusSet` unitSet b
		| t <- setToList (lookupset moves b)
		]
		
	bas = listToFM	
		[ (b, head as)
		| b <- addons
		, let as = setToList (cands b)
		, not (null as)
		]

	f x = lookupWithDefaultFM bas x x

	nobs = all `minusSet` mkSet (keysFM bas)

	a1 = keepstTNFA opts a nobs 
	a2 = mapTNFA opts f a1

    in

	trace ("Reuse.reuse1.moves = " ++ show moves) $
	trace ("Reuse.reuse1.bas = " ++ show bas) $
	trace ("Reuse.reuse1.nobs = " ++ show nobs) $

	a2



reuse opts a addons = fixpoint (\ b -> reuse1 opts b addons) a
	
