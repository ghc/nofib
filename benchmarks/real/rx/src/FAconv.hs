-- simple conversions

module FAconv 

( etnfa2tnfa, tnfa2etnfa
, tnfa2bnfa, bnfa2tnfa
, bdfa2bnfa, simplebnfa2bdfa
, bdfa2tnfa
)

where

import Set
import FiniteMap

import Stuff 
import Options

import FAtypes

import TA

---------------------------------------------------------------

etnfa2tnfa :: (Show a, Ord a) => Opts -> ETNFA a -> TNFA a
etnfa2tnfa opts a @ (ETNFA cons all starts moves eps) =
    let	ehull x = sethull (\ y -> lookupset eps y) x 
    	h x = ehull (unitSet x) `bind` lookupset moves
	moves1 = listToFM [ (x, h x) | x <- setToList all ]
	starts1 = mapSet unitSet starts `bind` ehull
    	t = TNFA cons all starts1 moves1
    in	
--	trace ("etnfa2tnfa.a = " ++ show a) $
--	trace ("etnfa2tnfa.t = " ++ show t) $
	t

--------------------------------------------------------------

tnfa2bnfa :: Ord a => Opts -> TNFA a -> BNFA a
tnfa2bnfa opts (TNFA cons all starts moves) =
    BNFA cons all starts (invert moves)


bnfa2tnfa :: Ord a => Opts -> BNFA a -> TNFA a
bnfa2tnfa opts (BNFA cons all starts moves) =
    TNFA cons all starts (invert moves)

tnfa2etnfa :: Ord a => Opts -> TNFA a -> ETNFA a
tnfa2etnfa opts(TNFA cons all starts moves) =
    ETNFA cons all starts moves emptyFM



bdfa2bnfa :: Ord a => Opts -> BDFA a -> BNFA a
bdfa2bnfa opts (BDFA cons all starts moves) =
    let	moves' = mapFM (\ x y -> unitSet y) moves
    in	BNFA cons all starts moves'


simplebnfa2bdfa opts (BNFA cons all starts moves) =
    let	moves1 = mapFM ( \ t ws -> 
		case setToList ws of 
			[w] -> w
			_ -> error "simplebnfa2bdfa" ) moves
    in 	BDFA cons all starts moves1

-------------------------------------------------------

bdfa2tnfa :: Opts -> BDFA Int -> TNFA Int
bdfa2tnfa opts = bnfa2tnfa opts . bdfa2bnfa opts

