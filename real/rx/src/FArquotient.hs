module FArquotient

( rquotientTNFA
, rquotientTNFApublic
)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAuseful
import FAkeepst

import FAcheat


import Trace

----------------------------------------------------------------------

rquotientTNFA :: Opts -> TCon -> TNFA Int -> TNFA Int -> TNFA Int
rquotientTNFA opts tc a1 @ (TNFA consa1 _ _ _) a2 @ (TNFA consa2 _ _ _) =
    let	
	-- not surprisingly, this is copied from intersectTNFA

	cons = consa1 
	TNFA cons1 all1 starts1 moves1 = a1
	TNFA cons2 all2 starts2 moves2 = a2

	comb (w1, w2) = mkSet
		[ mksterm (stcon t2) (zip (stargs t1) (stargs t2))
			-- don't use zippy here!
			-- we're mis-using tc of arity 0 slightly
		| t2 <- setToList 
		    (lookupWithDefaultFM moves2 (error "rquoteTNFA.t2") w2)
		, stcon t2 == tc || stcon t2 `elementOf` cons 

		, t1 <- setToList 
		    (lookupWithDefaultFM moves1 (error "rquoteTNFA.t1") w1)
		, stcon t2 == tc || stcon t1 == stcon t2

		]

	moves = listToFM [ ( (w1, w2), cs)
		| w1 <- setToList all1, w2 <- setToList all2 
		, cs <- [ comb (w1, w2) ], not (isEmptySet cs)
		]
	starts3 = mkSet [ (x, y) 
		| x <- setToList starts1, y <- setToList starts2 ]
	all3 =  mkSet [ (x, y) 
		| x <- setToList all1, y <- setToList all2 ]
	b3 = TNFA (cons `unionSet` unitSet tc) all3 starts3 moves

--	reachables = precs moves starts3
--	t4 @ (TNFA cons4 all4 starts4 moves4) = keepstTNFA opts b3 reachables

	t4 @ (TNFA cons4 all4 starts4 moves4) = usefulTNFA opts b3 -- ???

	starts5 = mkSet [ v1 
		| ((v1, v2) , ts ) <- fmToList moves4
		, or [ stcon t == tc | t <- setToList ts ]
		]
	b6 = TNFA cons1 all1 starts5 moves1

    in 	

--	trace ("\nrquotient.a1: " ++ show a1) $
--	trace ("\nrquotient.a2: " ++ show a2) $
--	trace ("\nrquotient.moves: " ++ show moves) $
--	trace ("\nrquotient.starts3: " ++ show starts3) $
--	trace ("\nrquotient.all3: " ++ show all3) $
--	trace ("\nrquotient.b3: " ++ show b3) $
--	trace ("\nrquotient.t4: " ++ show t4) $
--	trace ("\nrquotient.starts5: " ++ show starts5) $

	trinfo opts "rquotient" b6 $
	
	b6


rquotientTNFApublic :: Opts -> [TNFA Int] -> TNFA Int
rquotientTNFApublic opts args =
    if length args /= 3 
    then error "rquotientTNFApublic.args"
    else 
	let [tcarg, arg1, arg2] = args
	in  rquotientTNFA opts (cheat tcarg) arg1 arg2

