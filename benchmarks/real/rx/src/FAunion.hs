module FAunion

( unionTNFA
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
import FAconv

---------------------------------------------------------------------

unionTNFA :: Opts -> TNFA Int -> TNFA Int -> TNFA Int
unionTNFA opts x1 x2 =
    let	
	TNFA cons1 all1 starts1 moves1 = x1
	n1 = maximum (0 : (setToList all1)) + 1

	TNFA cons2 all2 starts2 moves2 = mapTNFA opts (n1 + )  x2

--	added 17-sep-98
-- 	this was sitting here for half a year at least.
--	find out why it is wrong!
--	top = maximum (0  : (setToList all2)) + 1

--	and why this is better:
	top = maximum (n1 : (setToList all2)) + 1

	cons = cons1 `unionSet` cons2

	y = ETNFA cons 
		((all1 `unionSet` all2) `unionSet` unitSet top)
		(unitSet top)
		(plusFM_C (error "unionTNFA") moves1 moves2)
		(unitFM top (starts1 `unionSet` starts2))

    	e = etnfa2tnfa opts y
	f = cmpctTNFA opts e


    in	

--	trace ("\nunionTNFA.x1 = " ++ show x1) $
--	trace ("\nunionTNFA.x2 = " ++ show x2) $
--	trace ("\nunionTNFA.n1 = " ++ show n1) $
--	trace ("\nunionTNFA.top = " ++ show top) $
--	trace ("\nunionTNFA.cons = " ++ show cons) $
--	trace ("\nunionTNFA.y = " ++ show y) $
--	trace ("\nunionTNFA.e = " ++ show e) $

	trinfo opts "union" f $

	f

-------------------------------------------------------------------------
