module FAkeepst

( keepstBNFA
, keepstTNFA
)


where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes

----------------------------------------------------------------------------

keepstBNFA :: Ord a => Opts -> BNFA a -> Set a -> BNFA a
keepstBNFA opts (BNFA cons all starts moves) keeps = 
    let starts' = (starts `intersectSet` keeps)
	moves' = mapFM (\ t v -> v `intersectSet` keeps) moves
	moves'' = filterFM (\ t v ->
		not (isEmptySet v)
		&& isEmptySet (mkSet (stargs t) `minusSet` keeps)) moves'
    in	BNFA cons  keeps starts' moves''

--------------------------------------------------------------------------

keepstBDFA :: Ord a => Opts -> BDFA a -> Set a -> BDFA a
keepstBDFA opts (BDFA cons all starts moves) keeps = 
    let starts' = (starts `intersectSet` keeps)
	moves'' = filterFM (\ t v -> 
		v `elementOf` keeps
		&& isEmptySet (mkSet (stargs t) `minusSet` keeps)) moves
    in	BDFA cons  keeps starts' moves''


---------------------------------------------------------------------------

keepstTNFA :: Ord a => Opts -> TNFA a -> Set a -> TNFA a
keepstTNFA opts (TNFA cons all starts moves) keeps = 
    let	starts' = (starts `intersectSet` keeps)
	rm s = filterSet 
		(\ t -> isEmptySet (mkSet (stargs t) `minusSet` keeps))
		s
	moves'' = filterFM (\ t v ->
		(t `elementOf` keeps)
		&& not (isEmptySet v)) moves
    in	TNFA cons keeps starts' moves''
