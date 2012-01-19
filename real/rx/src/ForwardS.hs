module ForwardS

( forwardS
, forwardSpublic
)

-- implements thomas genet's algorithm
-- for approximating term replacement in a finite automaton

-- we're looking at the system   S x y z   ->   x z (y z)

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
	ts = lookupWithDefaultFM moves (error "ForwardS.sons.ts") p
	lrs = 	[ (l, r) 
		| t <- setToList ts
		, tconname (stcon t) == "@"
		, let [l, r] = stargs t
		]
    in
	lrs



leaves :: TNFA Int -> Int -> [()]
leaves (TNFA cons all starts moves) p =
    let
	ts = lookupWithDefaultFM moves (error "ForwardS.leaves.ts") p
	lrs = 	[ () 
		| t <- setToList ts
		, tconname (stcon t) == "S"
		]
    in
	lrs



forwardS :: Opts -> TNFA Int -> TNFA Int
-- look for all matches of S x y z
-- add new states from that to x z (y z) 
forwardS opts a @ (TNFA cons all starts moves) =
    let	
	quads =	[ (t0, (x, y, z))
		| t0 <- setToList all
		, (t1, z) <- sons a t0
		, (t2, y) <- sons a t1
		, (t3, x) <- sons a t2
		, ()     <- leaves a t3	-- this looks for S
		]

	-- next free state
	next = 1 + maximum (setToList all)

	-- write new top state numbers to quads
	-- warnig: the number 2 depends on the states used in "new" below
	iquads = zip [next, next + 2 .. ] quads

	-- this is a bit ugly
	-- need to find the complete id information for the constructors
	-- we hope they are there
	ap = head [ con | con <- setToList cons, tconname con == "@" ]
	s  = head [ con | con <- setToList cons, tconname con == "S" ]
	
	-- generate new states per quad
	new (i, (t, (x, y, z))) = 
		[ (t    , mksterm ap [i + 0, i + 1]) 
		, (i + 0, mksterm ap [x, z]) 
		, (i + 1, mksterm ap [y, z]) 
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

	trinfo opts "forwardS" r'' $

	r''





forwardSpublic :: Opts -> [ TNFA Int ] -> TNFA Int

forwardSpublic opts args =
    if length args /= 1 
    then error "forwardSpublic.args"
    else 
	let [arg1] = args
	in  forwardS opts arg1



-- later:

-- iterate the forwardS operation
-- making the automaton deterministic and minimal
-- before and after each step
-- until process converges

