-- powerset construction

module FAdet

( bnfa2bdfa
, tnfa2bdfa
, detTNFA
)


where



import Set
import FiniteMap

import Options

import Stuff

import TA
import FAtypes

import FAconv

import FAcmpct

pick :: Ord a => FiniteMap (STerm a) (Set a) -> STerm (Set a) -> Set a
-- look up the (set of) predec. of a term (whose comps. are sets)
pick m t =
    let args = insts (stargs t) 
	ts = mapSet (\ a -> mksterm (stcon t) a) args
	ps = ts `bind` lookupset m 
    in  ps


phull :: Ord a =>
	TCons -> FiniteMap (STerm a) (Set a)	-- original map
	-> Set (Set a) -> Set (Set a) 		-- known/unknown
	-> FiniteMap (STerm (Set a)) (Set a)	-- input
	-> (Set (Set a), FiniteMap (STerm (Set a)) (Set a))	-- output

phull tcons m known unknown rels | isEmptySet unknown = (known, rels)
phull tcons m known unknown rels =
    let	ts = 	[ mksterm tc a
		| tc <- setToList tcons, n <- [tconarity tc], n > 0
		, a <- packs n 1 (setToList known) (setToList unknown)
		]

	ps = [ (t, p) | t <- ts, p <- [pick m t] -- new relations

-- x-perry-mental: don't generate the sink state {}
			, not (isEmptySet p)

	     ]				

	qs = listToFM ps			-- new relations as map
	gs = mkSet [ g | (t, g) <- ps ]		-- new sets
	ks = known `unionSet` unknown		-- they are no longer unknown
	ns = gs `minusSet` ks			-- these are brand new
	rs = plusFM_C (error "phull") rels qs	-- should not clash
    in	phull tcons m ks ns rs



{-# SPECIALIZE bnfa2bdfa' :: Opts -> BNFA Int -> BDFA (Set Int) #-}
bnfa2bdfa' :: Ord a => Opts -> BNFA a -> BDFA (Set a)
bnfa2bdfa' opts (BNFA cons all starts moves) =
    let	ps = 	[ ( mksterm tc []
		  , g
		  )
		| tc <- setToList cons, tconarity tc == 0

		, g <- [lookupset moves (mksterm tc [])] 

-- x-perry-mental: don't generate sink state {}
		, not (isEmptySet g)

		]
	qs = listToFM ps			-- start relations
	gs = mkSet [ g | (t, g) <- ps ]		-- start sets
	(ks, rs) = phull cons moves emptySet gs qs	-- find hull
	fs = filterSet (\ s -> not (isEmptySet (s `intersectSet` starts))) ks
    in	BDFA cons ks fs rs

------------------------------------------------------------------------

bnfa2bdfa :: (Show a, Ord a) => Opts -> BNFA a -> BDFA Int
bnfa2bdfa opts = cmpctBDFA opts . bnfa2bdfa' opts

tnfa2bdfa :: (Show a, Ord a) => Opts -> TNFA a -> BDFA Int
tnfa2bdfa opts = bnfa2bdfa opts . tnfa2bnfa opts


detTNFA :: Opts -> TNFA Int -> TNFA Int
detTNFA opts = bdfa2tnfa opts . tnfa2bdfa opts


