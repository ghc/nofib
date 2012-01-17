module CBackwardS

( cbackwardS
, cbackwardSpublic
)

-- checks whether a given grammar
-- is backward closed under reduction
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
	ts = lookupWithDefaultFM moves (error "CBackwardS.sons.ts") p
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
	ts = lookupWithDefaultFM moves (error "CBackwardS.leaves.ts") p
	lrs = 	[ () 
		| t <- setToList ts
		, tconname (stcon t) == "S"
		]
    in
	lrs



cbackwardS :: Opts -> TNFA Int -> TNFA Int
-- look back all matches of S x y z (successively)
-- add new states from that to x z (y z) 
cbackwardS opts a @ (TNFA cons all starts moves) =
    let	

        quads = [ (t, (ll, rl, rr))
                | t <- setToList all
                , (l, r) <- sons a t
                , (ll, lr) <- sons a l
                , (rl, rr) <- sons a r
                , lr == rr      -- these are the two z's
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

	ifn' t nxt =  
	    case setToList (lookupset imoves t) of
		[c] -> (c, t)	-- state is already there, and unique
		_ -> (nxt, t)	-- not there, or not unique


	ifap x y nxt = ifn' (mksterm ap [x, y]) nxt
	ifs      nxt = ifn' (mksterm s  [    ]) nxt

	-- generate new states per quad
	newsts (t, (x, y, z)) = 
	    let thes   @ (s  , ms  ) = ifs        (next + 0)
		thesx  @ (sx , msx ) = ifap  s   x (next + 1)
		thesxy @ (sxy, msxy) = ifap  sx  y (next + 2)
		( c, msxyz) 	     = ifap  sxy z (next + 3)
	    in
		if c == t 
		then []
		else
		[ (t       , msxyz)
		, thesxy
		, thesx
		, thes
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
		msg = "checking for contractum " ++ show txyz ++ ": "
			++ show m ++ "\n"
	    in
		( chose opts "tracecbackward" (trace msg) id )
		m


	diffs = foldr (unionTNFA opts) emptyTNFA 
		[ diff txyz | txyz <- quads ]

    in

	chose opts "tracecbackward" 
	(trace (   
		   "\ncbackward a: " ++ show a ++ "\n"
--		++ "\ncbackward na: " ++ show na ++ "\n" 
	))
	id $


	diffs






cbackwardSpublic :: Opts -> [ TNFA Int ] -> TNFA Int

cbackwardSpublic opts args =
    if length args /= 1 
    then error "cbackwardSpublic.args"
    else 
	let [arg1] = args
	in  cbackwardS opts arg1



