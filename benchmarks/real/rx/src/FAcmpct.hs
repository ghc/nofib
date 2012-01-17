module FAcmpct 

( cmpctBDFA
, cmpctTNFA
)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes

import FAmap

-------------------------------------------------------------

cmpctBDFA :: Ord a => Opts -> BDFA a -> BDFA Int
-- number states from 0 onwards
cmpctBDFA opts b @ (BDFA cons all starts moves) = 
    let	h = listToFM (zip (setToList all) [0..])
	f = lookupWithDefaultFM h (error "cmpctBDFA")
    in	mapBDFA opts f b


--cmpctTNFA :: Ord a => Opts -> TNFA a -> TNFA Int
-- number states from 0 onwards
cmpctTNFA opts b @ (TNFA cons all starts moves) = 
    let	h = listToFM (zip (setToList all) [0..])
	f x = lookupWithDefaultFM h 
		(error ("cmpctTNFA doesn't find: " ++ show x))
		x
    in	
--	trace ("\ncmpctTNFA.b : " ++ show b) $

	mapTNFA opts f b
