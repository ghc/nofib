module CForwardS

( cforwardS
, cforwardSpublic
)

-- checks whether a given grammar
-- is forward closed under reduction
-- in the system   S x y z   ->   x z (y z)

-- this implementation is ugly ugly ugly
-- w.r.t. the rest of the system
-- the reduction rule of S is hardwired
-- as are the names of the constructors (S and @)

where

import Trace

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAmin
import FAuseful

import FAneg
import FAintersect
import FAunion

-- import Reuse

sons :: TNFA Int -> Int -> [(Int, Int)]
sons (TNFA cons all starts moves) p =
    let
	ts = lookupWithDefaultFM moves (error "CForwardS.sons.ts") p
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
	ts = lookupWithDefaultFM moves (error "CForwardS.leaves.ts") p
	lrs = 	[ () 
		| t <- setToList ts
		, tconname (stcon t) == "S"
		]
    in
	lrs



cforwardS :: Opts -> TNFA Int -> TNFA Int
-- look for all matches of S x y z (successively)
-- add new states from that to x z (y z) 
cforwardS opts a @ (TNFA cons all starts moves) =
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


	-- this is a bit ugly
	-- need to find the complete id information for the constructors
	-- we hope they are there
	ap = head [ con | con <- setToList cons, tconname con == "@" ]
	s  = head [ con | con <- setToList cons, tconname con == "S" ]
	

	-- try to bypass complicate constructions.
	-- catch cases like this:
	-- X -> T X + something, Redex -> X S
	-- by looking up where (x z), (y z), (x z(y z))	
	-- may go in the current automaton 
	-- hoping it's deterministic

	imoves = invert moves

	ifn x y nxt = 
	    let xy = mksterm ap [x, y]
	    in case setToList (lookupset imoves xy) of
		[c] -> (c, xy)	-- state is already there, and unique
		_ -> (nxt, xy)	-- not there, or not unique


	-- generate new states per quad
	newsts (t, (x, y, z)) = 
	    let	thexz @ (xz, mxz) = ifn  x  z (next + 0)
		theyz @ (yz, myz) = ifn  y  z (next + 1)
		( c, mxzyz) = ifn xz yz (next + 2)
	    in
		if c == t 
		then []
		else
		[ (t       , mxzyz)
		, thexz
		, theyz
		]



	-- generate new moves for automaton

	red txyz @ (t, xyz) = TNFA cons all' starts moves'
	    where
		moves' = moves `mergeFM` 
			listToFM [ (a, unitSet t) | (a, t) <- newsts txyz ]
		all'= all `unionSet` mkSet (keysFM moves')


	-- compute differences: a with redex replaced \\ original a
	-- hack: first negate the input automaton a
	-- then intersect with rewritten automaton

	opts' = addToFM opts "trace" "on"
	na = negTNFA opts' a

	diff txyz @ (t, (x, y, z)) = 
	    let	
		r = red txyz 
		m = 	if r == a 
			then trace "\n*** shortcut ***" 
				emptyTNFA 
			else trace "\n*** longcut ***" 
				usefulTNFA opts $ intersectTNFA opts r na
		msg = "checking for redex " ++ show txyz ++ ": "
			++ show m ++ "\n"
	    in
		( chose opts "tracecforward" (trace msg) id )
		m


	diffs = foldr (unionTNFA opts) emptyTNFA 
		[ diff txyz | txyz <- quads ]

    in

	chose opts "tracecforward" 
	(trace (   
		   "\ncforward a: " ++ show a ++ "\n"
--		++ "\ncforward na: " ++ show na ++ "\n" 
	))
	id $


	diffs






cforwardSpublic :: Opts -> [ TNFA Int ] -> TNFA Int

cforwardSpublic opts args =
    if length args /= 1 
    then error "cforwardSpublic.args"
    else 
	let [arg1] = args
	in  cforwardS opts arg1



