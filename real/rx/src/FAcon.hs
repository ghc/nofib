module FAcon

( conTNFA
)

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

conTNFA :: Opts -> TCon -> [TNFA Int] -> TNFA Int
-- apply a constructor
conTNFA opts tc as = 
    let -- make each arg top-down, tag it with its number
	aks    = [ mapTNFA opts (\ v -> (k, v)) a 
		| (k, a) <- zippy [1..tconarity tc] as ]

	moves  = foldl 	(plusFM_C (error "conTNFA.moves")) emptyFM
			       [ m | TNFA _ _ _ m <- aks ]
	starts =               [ s | TNFA _ _ s _ <- aks ]
	alls   = unionManySets [ a | TNFA _ a _ _ <- aks ]
	cons   = unionManySets (unitSet tc : [ c | TNFA c _ _ _ <- aks ]) 

	top = (0,0); tops = unitSet top
	its = unitFM top (mapSet (mksterm tc) (insts starts))
	
	t = TNFA cons (alls `unionSet` tops) tops
		(plusFM_C (error "conTNFA.e") moves its)
	d = cmpctTNFA opts t
    in	

	trinfo opts "con" d $

	d

