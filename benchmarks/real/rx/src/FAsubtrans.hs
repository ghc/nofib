-- applies a transformation to exactly one of all states of an automaton
-- instance: reduce exactly one redex that may be situated aritrarily

module FAsubtrans


where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAmap
import FAcmpct

import FAuseful

--import Trace

--import List

subtransTNFA :: Opts -> (Opts -> TNFA Int -> TNFA Int) -> TNFA Int -> TNFA Int

subtransTNFA opts f a @ (TNFA cons all starts moves) =
    let
	n = maximum (setToList all)
	orig = n + 1	-- next unused state
	copy = n + 2	-- even more unused state

	o @ (TNFA ocons oall ostarts omoves) 
		= mapTNFA opts (\ m -> (orig, m)) a
	c @ (TNFA ccons call cstarts cmoves) 
		= mapTNFA opts (\ m -> (copy, m)) a

	-- stepping from the copy to the original:
	-- make exactly one argument point to the copy,
	-- all the others to the original
	-- that is, in the copy, there are no leaves here
	cmoves' = mapFM (\ w ts -> mkSet 
		[ mksterm tc (as' ++ b : cs')
		| t <- setToList ts
		, tc <- [ stcon t ], args <- [ stargs t ]
		, (as, b : cs) <- zip (inits args) (tails args)
		, as' <- [[ (orig, a) | (_, a) <- as ]]
		, cs' <- [[ (orig, c) | (_, c) <- cs ]]
		] ) cmoves

	-- the new automata
	ns = listToFM 
		[ (w, mapTNFA opts (\ m -> (w, m))
			(f opts (usefulTNFA opts	-- does this help?
				(TNFA cons all (unitSet w) moves))))
		| w <- setToList all
		]

	ncons  = unionManySets [ cons  | TNFA cons _ _ _ <- eltsFM ns ]
	nall   = unionManySets [ all   | TNFA _ all  _ _ <- eltsFM ns ]

	-- the moves in them
	mmoves = foldl (plusFM_C unionSet) emptyFM 
		[ moves | TNFA _ _ _ moves <- eltsFM ns ]

	-- the moves to them
	nmoves = listToFM 
		[ ( (copy, w) , nstarts `bind` lookupset nmoves )
		| w <- setToList all
		, TNFA _ _ nstarts nmoves <- 
			[ lookupWithDefaultFM ns (error "subtransTNFA.ns") w ]
		]

	-- all together now
	cons' = cons `unionSet` ncons
	all' = oall `unionSet` call `unionSet` nall

	starts' = cstarts
	moves' = plusFM_C unionSet 
			(plusFM_C unionSet nmoves mmoves)
			(plusFM_C unionSet omoves cmoves')

	d = TNFA cons' all' starts' moves'
	e = cmpctTNFA opts d

    in

--	trace ("\nFAsubtrans.a = " ++ show a) $
--	trace ("\nFAsubtrans.o = " ++ show o) $
--	trace ("\nFAsubtrans.c = " ++ show c) $
--	trace ("\nFAsubtrans.d = " ++ show d) $
--	trace ("\nFAsubtrans.e = " ++ show e) $

	e

