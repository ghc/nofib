module BackwardS

( backwardS
, backwardSpublic
)

-- implements thomas genet's algorithm
-- for approximating term replacement in a finite automaton

-- we're looking at the reversed system   x z (y z) -> S x y z

-- this implementation is ugly ugly ugly
-- w.r.t. the rest of the system
-- the reduction rule of S is hardwired
-- as are the names of the constructors (S and @)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import Reuse

sons :: TNFA Int -> Int -> [(Int, Int)]
sons (TNFA cons all starts moves) p =
    let
	ts = lookupWithDefaultFM moves (error "BackwardS.sons.ts") p
	lrs = 	[ (l, r) 
		| t <- setToList ts
		, tconname (stcon t) == "@"
		, let [l, r] = stargs t
		]
    in
	lrs


backwardS :: Opts -> TNFA Int -> TNFA Int
-- look for all matches of x z (y z) 
-- add new states from that to S x y z
backwardS opts a @ (TNFA cons all starts moves) =
    let	
	quads =	[ (t, (ll, rl, rr))
		| t <- setToList all
		, (l, r) <- sons a t
		, (ll, lr) <- sons a l
		, (rl, rr) <- sons a r
		, lr == rr	-- these are the two z's
		]

	-- next free state
	next = 1 + maximum (setToList all)

	-- write new top state numbers to quads
	-- warnig: the number 3 depends on the states used in "new" below
	iquads = zip [next, next + 3 .. ] quads

	-- this is a bit ugly
	-- need to find the complete id information for the constructors
	-- we hope they are there
	ap = head [ con | con <- setToList cons, tconname con == "@" ]
	s  = head [ con | con <- setToList cons, tconname con == "S" ]
	
	-- generate new states per quad

	movesr = invert moves

	new (i, (t, (x, y, z))) = 
		[ (t    , mksterm ap [i + 0, z] )
		, (i + 0, mksterm ap [i + 1, y] )
		, (i + 1, mksterm ap [i + 2, x] )
		, (i + 2, mksterm s  []         )
		]

	newsl = [ p | iq <- iquads, p <- new iq ]
	news = listToFM [ (a, unitSet t) | (a, t) <- newsl ]
	moves' = moves `mergeFM` news
	all' = all `unionSet` mkSet (keysFM moves')

	r = TNFA cons all' starts moves'


	addons = [ a | a <- keysFM news, a >= next ]
	r' = reuse opts r addons

	r'' = chose opts "reuse" r' r

    in

	trinfo opts "backwardS" r'' $

	r''




backwardSpublic :: Opts -> [ TNFA Int ] -> TNFA Int

backwardSpublic opts args =
    if length args /= 1 
    then error "backwardSpublic.args"
    else 
	let [arg1] = args
	in  backwardS opts arg1



-- later:

-- iterate the backwardS operation
-- making the automaton deterministic and minimal
-- before and after each step
-- until process converges

-- making determin. should ensure that the two z's really "are the same"
