-- finite automata on trees (arbitrary term algebras)

module FA

( Auto

-- export something

, e2d
, t2d

, d2t
, d2e


, fids		-- identifiers
, hsTNFA	-- possible default operations

-- above this line, eveyrthing is fine, abstract, and so on
-- below is some stuff that is exported 
-- because the module structure isn't quite right


, TNFA(..)	-- todo: make abstract


)

where

import Set
import FiniteMap

import Options	-- may modify behaviour

import Sorters


import TA -- term algebra

import Ids
import Syntax

import Stuff

import FAtypes
import FAconv

import FAuseful
import FAunify

import FAdet
import FAmin

import FAunion
import FAintersect
import FAcon
import FAminus

import FAtimes
import FAstar
import FArquotient
import FAlquotient



import ForwardS
import CForwardS

import BackwardS
import CBackwardS

import SaturnS

import Instance

-- import CloseS

-----------------------------------------------------------------------

-- operations that are probably used often

e2d :: (Show a, Ord a) => Opts -> ETNFA a -> BDFA Int
e2d opts = tnfa2bdfa opts . etnfa2tnfa opts



t2d :: (Show a, Ord a) => Opts -> TNFA a -> BDFA Int
t2d opts = tnfa2bdfa opts

d2t :: (Show a, Ord a) => Opts -> BDFA a -> TNFA a
d2t opts =              bnfa2tnfa opts . bdfa2bnfa opts

d2e :: (Show a, Ord a) => Opts -> BDFA a -> ETNFA a
d2e opts = tnfa2etnfa opts . bnfa2tnfa opts . bdfa2bnfa opts

----------------------------------------------------------------------------



fids :: [ (Id, Opts -> [TNFA Int] -> TNFA Int) ]
fids =
 	[ 	( mkid "++" (Passive "++") (Just 2) Op Op (Just 30) Lft
		, \ opts -> foldl1 (unionTNFA opts) )

		-- cannot use "--" because that's a comment
	, 	( mkid "\\\\" (Passive "\\\\") (Just 2) Op Op (Just 40) Lft
		, \ opts -> foldr1 (minusTNFA opts) )

	, 	( mkid "&"  (Passive "&") (Just 2) Op Op (Just 50) Lft
		, \opts -> foldl1 (intersectTNFA opts) )

	, 	( mkid "->" (Passive "\\longrightarrow") (Just 2) Op Op (Just 20) Lft
		, error "never evaluate fids.(->)" )

	, 	( mkid ";"  (Passive ";") (Just 2) Op Op (Just 10) Lft
		-- todo: this is the wrong place
		, error "never evaluate (;)" )
	, 	( mkid "=" (Passive "=") (Just 2) Op Op (Just 15) Lft
		-- todo: this is the wrong place
		, error "never evaluate (=)" )

	,	( userfun 1 "det"
		, \ opts [x] -> detTNFA opts x )
	,	( userfun 1 "min"
		, \ opts [x] -> minTNFA opts x )
	,	( userfun 1 "useful" 
		, \ opts [x] -> usefulTNFA opts x )
	,	( userfun 1  "unify"
		, \ opts [x] -> unifyTNFA opts x )

	,	( userfun 3 "times"
		, \ opts xs -> timesTNFApublic opts xs )
	,	( userfun 3 "star"
		, \ opts xs -> starTNFApublic opts xs )
	,	( userfun 3 "rquotient"
		, \ opts xs -> rquotientTNFApublic opts xs )
	,	( userfun 3 "lquotient"
		, \ opts xs -> lquotientTNFApublic opts xs )

	,	( userfun 1 "forwardS"
		, \ opts xs -> forwardSpublic opts xs )
	,	( userfun 1 "cforwardS"
		, \ opts xs -> cforwardSpublic opts xs )

	,	( userfun 1 "backwardS"
		, \ opts xs -> backwardSpublic opts xs )
	,	( userfun 1 "cbackwardS"
		, \ opts xs -> cbackwardSpublic opts xs )


	,	( userfun 1 "saturnS"
		, \ opts xs -> saturnSpublic opts xs )

	,	( userfun 1 "inst"
		, \ opts xs -> instpublic opts xs )


-- broken
--	,	( userfun 1 "closeS"
--		, \ opts xs -> closeSpublic opts xs )
	
	]

-- some transformations (that keep the meaning)
-- most imortant (costly) first
hsTNFA = ["min","det","useful","unify"]



