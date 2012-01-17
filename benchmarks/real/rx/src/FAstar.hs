module FAstar

( starTNFA
, starTNFApublic 
)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes

import Ids

import FAmap

import FAcheat


---------------------------------------------------------------------------

starTNFA :: Opts -> TCon -> TNFA Int -> TNFA Int -> TNFA Int
-- star dot product of two langugaes.
-- replaces one specified nullary constructor of the first language
-- with an epsilon trasition to the second language
-- or with an epsilon to the first lang's start
starTNFA opts tc
	a @ (TNFA cons1 all1 starts1 moves1)
	b =
    let
	startmoves1 = starts1 `bind` (lookupset moves1)

	m = 1 + maximum (0 :  setToList all1)
	TNFA cons2 all2 starts2 moves2 = mapTNFA opts (\ n -> n + m) b

	-- all that can be constructed from the start
	startmoves2 = starts2 `bind` (lookupset moves2)

	startmoves = startmoves1 `unionSet` startmoves2

	change t = if stcon t == tc then startmoves else unitSet t

	moves3 = mapFM (\ v ts -> ts `bind` change) moves1
	
	cons = (cons1 `minusSet` unitSet tc) `unionSet` cons2 
	all = all1 `unionSet` all2
	starts = starts2 `unionSet` starts1
	moves = plusFM_C (error "starTNFA.moves") moves3 moves2

	c = TNFA cons all starts moves

    in	

	trinfo opts "star" c $

	c


starTNFApublic :: Opts -> [TNFA Int] -> TNFA Int
starTNFApublic opts args =
    if length args /= 3 
    then error "starTNFApublic.args"
    else 
	let [tcarg, arg1, arg2] = args
	in  starTNFA opts (cheat tcarg) arg1 arg2

