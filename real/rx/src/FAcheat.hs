module FAcheat

( cheat
)

where


import Set
import FiniteMap

import TA
import FAtypes

import Stuff


--------------------------------------------------------------------

-- we cheat a bit. the constructor
-- is acutally given as trivial automaton

cheat :: TNFA Int -> TCon
cheat (TNFA cons all starts moves) = 
    let	prod = setToList (starts `bind` lookupset moves)
    in  if length prod /= 1
	    then error "cheat.prod"
	    else
		let [t] = prod; tc = stcon t
		in  if tconarity tc /= 0
		    then error "timesTNFApublic.tc"
		    else tc
