module FAunify

( unifyTNFA
)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAhom
import FAcmpct


unifyTNFA :: Opts -> TNFA Int -> TNFA Int
unifyTNFA opts = fixpoint (same opts) where
	same opts b @ (TNFA cons all starts moves) =
	    let	-- this uses Ord on sets!
		c = collectFM (eltsFM moves)
		h = mapFM ( \ w ts -> 
		    lookupWithDefaultFM c 
			(error ("same.c cannot find " ++ show ts)) ts) moves
	    	d = homTNFA opts (\ x -> case lookupFM h x of
			Just y -> Right y; Nothing -> Left x) b 
		e = cmpctTNFA opts d
	    in

--		trace ("(* heurist *)") $

--		trace ("\nheuristic.same.b: " ++ show b) $
--		trace ("\nheuristic.same.c: " ++ show c) $
--		trace ("\nheuristic.same.h: " ++ show h) $
--		trace ("\nheuristic.same.d: " ++ show d) $
--		trace ("\nheuristic.same.e: " ++ show d) $

		trinfo opts "unify" e $

		e

