module FAminus

( minusTNFA
)

where


import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAintersect

import FAneg

----------------------------------------------------------------------

minusTNFA :: Opts -> TNFA Int -> TNFA Int -> TNFA Int
minusTNFA opts
	  x1 @ (TNFA cons1 all1 starts1 moves1)
	  x2 @ (TNFA cons2 all2 starts2 moves2) =
    let	cons = cons1 `unionSet` cons2
	y2 = TNFA cons all2 starts2 moves2
    	z2 = negTNFA opts y2
	v  = intersectTNFA opts x1 z2
    in	
	trinfo opts "minus" v $
	v

