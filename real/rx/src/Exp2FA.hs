module Exp2FA

( grammar2etnfa
)

where

import Set
import FiniteMap

import Grammar

import TA
import FAtypes

import Stuff
import Options

import Ids

------------------------------------------------------------------------

grammar2etnfa :: (Show a, Ord a) => Opts -> Grammar a -> ETNFA a
grammar2etnfa opts (start, rules) =
    let
	all = unionManySets ( unitSet start
		:  [ mkSet (v : stargs t) | (v, Right t) <- rules ] 
		++ [ mkSet (v : [w])      | (v, Left w ) <- rules ] )
	moves = addListToFM_C unionSet emptyFM
		[ (v, unitSet t) | (v, Right t) <- rules ]
	eps =  addListToFM_C unionSet emptyFM
		[ (v, unitSet w) | (v, Left w) <- rules ]
	cons = mkSet [ stcon t | (v, Right t) <- rules ]
	e = ETNFA cons all (unitSet start) moves eps
    in

--	trace ("\ngrammar2etnfa.e = " ++ show e) $

	e
