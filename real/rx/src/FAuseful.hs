module FAuseful

( prods, precs
, usefulBDFA, usefulTNFA
) 

where


import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes

import FAconv

import FAkeepst

---------------------------------------------------------------------------

-- producers: those that ->> leaves

prods :: Ord a => TCons -> FiniteMap (STerm a) (Set a) -> Set a
prods tcons m =

    let	ls = unionManySets -- find those that produce leaves
		[ lookupset m (mksterm tc [])
		| tc <- setToList tcons, tconarity tc == 0
		]

	prhull known unknown | isEmptySet unknown = known
	prhull known unknown =
	    let ps = unionManySets
			[ lookupset m (mksterm tc a)
			| tc <- setToList tcons, n <- [tconarity tc], n > 0
			, a <- packs n 1 (setToList known) (setToList unknown)
			]
		ks = known `unionSet` unknown
		qs = ps `minusSet` ks
	    in prhull ks qs

    in	prhull emptySet ls

------------------------------------------------------------------------

-- produceds: those that start ->> .

precs :: Ord a => FiniteMap a (Set (STerm a)) -> Set a -> Set a
precs m starts =
--    let	h x = lookupWithDefaultFM m (error "precs") x
    let	h x = lookupset m  x
		`bind` \ t -> mkSet (stargs t)
    in	sethull h starts

------------------------------------------------------------------------

usefulBDFA :: (Show a, Ord a) => Opts -> BDFA a -> BDFA a
usefulBDFA opts e1 =
    let e2 @ (BNFA cons2 all2 starts2 moves2) = bdfa2bnfa opts e1
	qs = prods cons2 moves2
	e3 = keepstBNFA opts e2 qs
	e4 @ (TNFA cons4 all4 starts4 moves4) = bnfa2tnfa opts e3
	ps = precs moves4 starts4
	e5 = keepstTNFA opts e4 ps
	e6 = tnfa2bnfa opts e5
	e7 = simplebnfa2bdfa opts e6
    in

--	trace ("\nuseful.e1 = " ++ show e1) $
--	trace ("\nuseful.e2 = " ++ show e2) $
--	trace ("\nuseful.qs = " ++ show qs) $
--	trace ("\nuseful.e3 = " ++ show e3) $
--	trace ("\nuseful.e4 = " ++ show e4) $
--	trace ("\nuseful.ps = " ++ show ps) $
--	trace ("\nuseful.e5 = " ++ show e5) $
--	trace ("\nuseful.e6 = " ++ show e6) $
--	trace ("\nuseful.e7 = " ++ show e7) $

	e7

----------------------------------------------------------

usefulTNFA :: (Show a, Ord a) => Opts -> TNFA a -> TNFA a
-- keep only those states that produce leaves
-- and that are reachable from the start
usefulTNFA opts e1 =
    let	
	e2 @ (BNFA cons2 all2 starts2 moves2) = tnfa2bnfa opts e1
	qs = prods cons2 moves2
	e3 = keepstBNFA opts e2 qs
	e4 @ (TNFA cons4 all4 starts4 moves4) = bnfa2tnfa opts e3
	ps = precs moves4 starts4
	e5 = keepstTNFA opts e4 ps
    in

--	trace ("\nuseful.e1 = " ++ show e1) $
--	trace ("\nuseful.e2 = " ++ show e2) $
--	trace ("\nuseful.qs = " ++ show qs) $
--	trace ("\nuseful.e3 = " ++ show e3) $
--	trace ("\nuseful.e4 = " ++ show e4) $
--	trace ("\nuseful.ps = " ++ show ps) $
--	trace ("\nuseful.e5 = " ++ show e5) $

	e5
