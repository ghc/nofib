module FAintersect

( intersectTNFA
)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAcmpct
import FAkeepcons

intersectTNFA :: Opts -> TNFA Int -> TNFA Int -> TNFA Int
intersectTNFA opts a1 @ (TNFA consa1 _ _ _) a2 @ (TNFA consa2 _ _ _) =
    let	cons = consa1 `intersectSet` consa2
	TNFA cons1 all1 starts1 moves1 = keepconsTNFA opts a1 cons
	TNFA cons2 all2 starts2 moves2 = keepconsTNFA opts a2 cons

	comb (w1, w2) = mkSet
		[ mksterm (stcon t1) (zippy (stargs t1) (stargs t2)) 
		| t1 <- setToList 
		    (lookupWithDefaultFM moves1 (error "intersectTNFA.t1") w1)
		, stcon t1 `elementOf` cons

		, t2 <- setToList 
		    (lookupWithDefaultFM moves2 (error "intersectTNFA.t2") w2)

		, stcon t2 `elementOf` cons
		, stcon t1 == stcon t2
		]

	moves = listToFM [ ( (w1, w2), cs)
		| w1 <- setToList all1, w2 <- setToList all2 
		, cs <- [ comb (w1, w2) ], not (isEmptySet cs)
		]
	starts3 = mkSet [ (x, y) 
		| x <- setToList starts1, y <- setToList starts2 ]

	all3 =  mkSet [ (x, y) 
		| x <- setToList all1, y <- setToList all2 ]
	b3 = TNFA cons all3 starts3 moves

	c = cmpctTNFA opts b3

    in 	
--	trace ("\nintersectTNFA.a1: " ++ show a1) $
--	trace ("\nintersectTNFA.a2: " ++ show a2) $
--	trace ("\nintersectTNFA.cons: " ++ show cons) $
--	trace ("\nintersectTNFA.moves: " ++ show moves) $
--	trace ("\nintersectTNFA.starts': " ++ show starts') $
--	trace ("\nintersectTNFA.all: " ++ show all) $
--	trace ("\nintersectTNFA.starts: " ++ show starts) $
--	trace ("\nintersectTNFA.b: " ++ show b) $

	trinfo opts "intersect" c $

	c
