module FA2Exp

( etnfa2exp
, tnfa2exp

, foldnonrec
) 

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import FAconv
import FA

import Ids
import Syntax

tnfa2exp :: (Show a, Ord a) => Opts -> TNFA a -> Exp
tnfa2exp opts = etnfa2exp opts . tnfa2etnfa opts



-----------------------------------------------------------------------


etnfa2exp :: (Ord a, Show a) => Opts -> ETNFA a -> Exp
etnfa2exp opts (ETNFA cons all starts moves eps) =
    let
	-- todo: this neither nice nor correct nor in the right place
	-- (the user might have overridden the latex format entry)

	plus = head [ id | (id, _) <- fids, idname id == "++" ]

	expset [] = Coll CSet []
	expset [x] = x
	expset xs = foldl1 (\ x y -> App plus [x, y]) xs

	leadsto = head [ id | (id, _) <- fids, idname id == "->" ]

	eall = mapSet var2id all
	estarts = expset (map var2exp (setToList starts))
	emoves = [ ( var2exp x
		 , expset (  map sterm2exp (setToList ts)
		       ++ map var2exp (setToList (lookupset eps x))) )
	       | (x, ts) <- fmToList moves
	       ]
	
	(cstarts, cmoves) =
	    (chose opts "foldnonrec" (foldnonrec eall) id) $
	    (chose opts "foldconst" (foldconst eall) id) $
		(estarts, emoves)


    in 	if (null cmoves && onoff opts "hidegrammar")
	then cstarts
	else
		 App (userfun 2 "grammar")		-- todo: wrong place
		[ cstarts
		, Coll CSet [ App leadsto [ x, y ] 
			| (x, y) <- cmoves ]
		]

---------------------------------------------------------------------------

varset r = mkSet (appids r)
varsets xrs = unionManySets [ varset r | (x, r) <- xrs ]

substMoves name val moves = 
    [ (x, substExp name val r) | (x, r) <- moves ]

foldconst vars (starts, moves) =
    fixpoint (\ (starts, moves) ->
	case 	[ (x, r) | (x, r) <- moves 
		, isEmptySet 	(varset r `intersectSet` vars)
		] of
	    [] -> (starts, moves)
	    (x, r) : _ -> 

--		trace ("\nfoldconst " ++ show x ++ " => " ++ show r) $


		( substExp x r starts
		, substMoves x r [ (y, s) | (y, s) <- moves, y /= x ] ) )
	(starts, moves)

------------------------------------------------------------------------
	    
foldnonrec vars (starts, moves) =
    fixpoint (\ (starts, moves) ->
	case 	[ (x, r) | (x, r) <- moves 
		, not (unAppId x `elementOf` varset r)
		] of
	    [] -> (starts, moves)
	    (x, r) : _ -> 

--		trace ("\nfoldnonrec " ++ show x ++ " => " ++ show r) $


		( substExp x r starts
		, substMoves x r [ (y, s) | (y, s) <- moves, y /= x ] ) )
	(starts, moves)
	    


