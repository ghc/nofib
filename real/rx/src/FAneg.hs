module FAneg

( negTNFA
)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes

import FAconv

import FAdet

-------------------------------------------------------------------------

negTNFA :: Opts -> TNFA Int -> TNFA Int
negTNFA opts x =
    let BDFA cons all starts moves =  tnfa2bdfa opts x
	sink = 1 + maximum (0 : setToList all)
	all1 = unitSet sink `unionSet` all
	starts1 = all1 `minusSet` starts
	moves1 = listToFM 
		[ (t, lookupWithDefaultFM moves sink t)
		| tc <- setToList cons, n <- [tconarity tc]
		, args <- setToList (insts (take n (repeat all1)))
		, t <- [ mksterm tc args ]
		]
    	d = BDFA cons all1 starts1 moves1
	u = bnfa2tnfa opts (bdfa2bnfa opts d)
    in	
	trinfo opts "neg" u $
	u

