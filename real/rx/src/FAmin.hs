module FAmin 

( minBDFA
, minTNFA
)

where


import Set
import FiniteMap

import Options

import Stuff

import TA
import FAtypes
import Ids

import FAconv

import FAdet
import FAhom

-- amin stuff

partFM :: Ord a => [[a]] -> FiniteMap a Int
-- input is list of lists that constitute a partition
-- output map each elem to the number of its class
partFM p = 
    addListToFM_C 
	(\ _ _ -> error "partFM") -- paranoid: check uniq-ness
	emptyFM
	[ (v, k) | (k, vs) <- zip [0..] p, v <- vs ]

-- items :: [a] -> [(Int, a, [a])]
-- lists with dropped/picked nth element
items [] = []
items (x : xs) = (0, x, xs) : [ (n + 1, y, x : ys) | (n, y, ys) <- items xs ]

refineFM :: (Show a, Ord a) =>
	FiniteMap a Int -> FiniteMap a Int -> FiniteMap a Int
-- uses collectFM: result number range may have holes
-- f must be a complete map, g may have holes
refineFM f g =
    let	fg = mapFM ( \ x fx -> 
		(fx, lookupFM g x)) 
		f
	p  = collectFM (eltsFM fg)
	h  = mapFM ( \ x fgx -> 
		lookupWithDefaultFM p (error "refineFM.h") fgx ) 
		fg
    in

--	trace ("\nrefineFM.fg = " ++ show fg) $
--	trace ("\nrefineFM.p  = " ++ show p ) $
--	trace ("\nrefineFM.h  = " ++ show h ) $

	h		


refineFMs :: (Show a, Ord a) => 
	FiniteMap a Int -> [ FiniteMap a Int ] -> FiniteMap a Int
refineFMs p [] = p
refineFMs p (f : fs) =
    	if sizeFM p == cardinality (mkSet (eltsFM p))
    	then p 					-- cannot be further refined
    	else refineFMs (refineFM p f) fs 	
    

tconthrough :: (Show a, Ord a)
	=> FiniteMap (STerm a) a -- transition table
	-> Set a 		-- all variables
	-> FiniteMap a Int	-- a partition of all variables
	-> FiniteMap a Int	-- refinement of that partition

tconthrough m all p =
    let	tups (t, w) = 
		[ ( ( stcon t, n, xs )
		  , (x, lookupWithDefaultFM p (error "tconthrough.w") w) )
		| (n, x, xs) <- items (stargs t)
		]

	h = addListToFM_C 
		(\ a b -> plusFM_C (error "tconthrough.h") a b) 
		emptyFM 
		[ (fun, unitFM x w) 
		| tw <- fmToList m, (fun, (x, w)) <- tups tw 
		]

    	q = refineFMs p (eltsFM h)
    in	

--	trace ("\ntconthrough.p: " ++ show p) $
--	trace ("\ntconthrough.h: " ++ show h) $
--	trace ("\ntconthrough.q: " ++ show q) $
	
	q

------------------------------------------------------------------------

minBDFA :: (Show a, Ord a) => Opts -> BDFA a -> BDFA Int
minBDFA opts b @ (BDFA cons all starts moves) =
    let	nostarts = all `minusSet` starts
	p = partFM [ setToList starts, setToList nostarts ]
	q = fixpoint (tconthrough moves all) p
	f = lookupWithDefaultFM q (error "bdfamin.f")
	c = homBDFA opts f b
    in	
--	trace ("\nbdfamin.nostarts: " ++ show nostarts) $
--	trace ("\nbdfamin.p: " ++ show p) $
--	trace ("\nbdfamin.q: " ++ show q) $
--	trace ("\nbdfamin.c: " ++ show c) $
	c


minTNFA :: Opts -> TNFA Int -> TNFA Int
minTNFA opts = bdfa2tnfa opts . minBDFA opts . tnfa2bdfa opts



