module FAlquotient

( lquotientTNFA
, lquotientTNFApublic
)

where


import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAuseful (prods)

import FAcheat

----------------------------------------------------------------------

lquotientTNFA :: Opts -> TCon -> TNFA Int -> TNFA Int -> TNFA Int
lquotientTNFA opts tc a1 @ (TNFA consa1 _ _ _) a2 @ (TNFA consa2 _ _ _) =
    let	
	-- not surprisingly, this is copied from intersectTNFA

	-- todo:check that tc not in cons1

	cons = consa1 `unionSet` unitSet tc

	TNFA cons1 all1 starts1 moves1 = a1
	TNFA cons2 all2 starts2 moves2 = a2

	comb (w1, w2) = mkSet
		[ mksterm (stcon t2) (zippy (stargs t1) (stargs t2))
		| t2 <- setToList 
		    (lookupWithDefaultFM moves2 (error "lquoteTNFA.t2") w2)

		, t1 <- setToList 
		    (lookupWithDefaultFM moves1 (error "lquoteTNFA.t1") w1)
		, stcon t2 == stcon t1

		]

	moves = listToFM [ ( (w1, w2), cs)
		| w1 <- setToList all1, w2 <- setToList all2 
		, cs <- [ comb (w1, w2) ]	
		, not (isEmptySet cs)
		]

	moves3 = invert moves
	prods3 = prods cons2 moves3	-- those that produce leaves

	ws = prods3 `bind` \ (w1, w2) ->  -- mark their partners
		if w2 `elementOf` starts2 then unitSet w1 else emptySet

	moves4 = mapFM (\ w ts -> 
		if w `elementOf` ws 
		then ts `unionSet` unitSet (mksterm tc [])
		else ts ) moves1

	b3 = TNFA cons all1 starts1 moves4
	

    in 	

--	trace ("\nlquotient.a1: " ++ show a1) $
--	trace ("\nlquotient.a2: " ++ show a2) $
--	trace ("\nlquotient.moves: " ++ show moves) $
--	trace ("\nlquotient.moves3: " ++ show moves3) $
--	trace ("\nlquotient.prods3: " ++ show prods3) $
--	trace ("\nlquotient.ws: " ++ show ws) $
--	trace ("\nlquotient.moves4: " ++ show moves4) $
--	trace ("\nlquotient.b3: " ++ show b3) $

	trinfo opts "lquotient" b3 $

	b3


lquotientTNFApublic :: Opts -> [TNFA Int] -> TNFA Int
lquotientTNFApublic opts args =
    if length args /= 3 
    then error "lquotientTNFApublic.args"
    else 
	let [tcarg, arg1, arg2] = args
	in  lquotientTNFA opts (cheat tcarg) arg1 arg2


