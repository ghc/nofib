module SaturnS

( saturnS
, saturnSpublic
)

-- checks whether a given grammar
-- is forward saturated under reduction
-- in the system   S x y z   <->   x z (y z)

-- i. e. each state that produces a redex
-- must also produce its contractum (and back)


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

import FAcon

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



stackit txyzs = addListToFM_C unionSet emptyFM 
	[ (xyz, unitSet t) | (t, xyz) <- txyzs ]



saturnS :: Opts -> TNFA Int -> TNFA Int
-- look for all matches of S x y z (successively)
-- add new states from that to x z (y z) 
saturnS opts a @ (TNFA cons all starts moves) =
    let	

	redexquads =	[ (t0, (x, y, z))
		| t0 <- setToList all
		, (t1, z) <- sons a t0
		, (t2, y) <- sons a t1
		, (t3, x) <- sons a t2
		, ()     <- leaves a t3	-- this looks for S
		]

	redexes = stackit redexquads


        contraquads = [ (t0, (ll, rl, rr))
                | t0 <- setToList all
                , (l, r) <- sons a t0
                , (ll, lr) <- sons a l
                , (rl, rr) <- sons a r
                , lr == rr      -- these are the two z's
                ]

	contras = stackit contraquads

	purgeFM = filterFM (\ k e -> not (isEmptySet e))


	nof = purgeFM $ addListToFM_C minusSet redexes (fmToList contras)
	nob = purgeFM $ addListToFM_C minusSet contras (fmToList redexes)
	
	nofs = (keysFM nof)
	nobs = (keysFM nob)

	-- this is a bit ugly
	-- need to find the complete id information for the constructors
	-- we hope they are there
	ap = head [ con | con <- setToList cons, tconname con == "@" ]
	s  = head [ con | con <- setToList cons, tconname con == "S" ]


 	-- next free state
	next = 1 + maximum (setToList all)


	mksterm' c args = unitSet (mksterm c args)

	mkredex (x, y, z) = listToFM
		[ (next + 0, mksterm' ap [next + 3, z])
		, (next + 1, mksterm' s [])
		, (next + 2, mksterm' ap [next + 1, x])
		, (next + 3, mksterm' ap [next + 2, y])
		]

	mkcontra (x, y, z) = listToFM
		[ (next + 0, mksterm' ap [next + 1, next + 2])
		, (next + 1, mksterm' ap [x, z])
		, (next + 2, mksterm' ap [y, z])
		]

	nofx = [ ("no contractum for:"
		, unitSet next
		 , plusFM_C (error "SaturnS.nofs") moves (mkredex xyz)
		 )
		| xyz <- nofs ]

	nobx = [ ("no redex for:" 
		, unitSet next
		, plusFM_C (error "SaturnS.nobs") moves (mkcontra xyz)
		)
		| xyz <- nobs ]

	okeh = [ ("everything closed", starts, emptyFM) ]
	
	(msg, starts', moves') = head (nofx ++ nobx ++ okeh)

    in

	trace ("redexes: " ++ show redexes) $
	trace ("contras: " ++ show contras) $

	trace ("redexes w/o contra: = " ++ show nofs) $
	trace ("contras w/o redex : = " ++ show nobs) $

	trace ("SaturnS : " ++ msg) $

	TNFA cons (all `unionSet` mkSet (keysFM moves')) starts' moves'






saturnSpublic :: Opts -> [ TNFA Int ] -> TNFA Int

saturnSpublic opts args =
    if length args /= 1 
    then error "saturnSpublic.args"
    else 
	let [arg1] = args
	in  saturnS opts arg1



