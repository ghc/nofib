module FAtimes

( timesTNFA
, timesTNFApublic
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

timesTNFA :: Opts -> TCon -> TNFA Int -> TNFA Int -> TNFA Int
-- dot product of two langugaes.
-- replaces one specified nullary constructor of the first language
-- with an epsilon trasition to the second language
timesTNFA opts tc
	a @ (TNFA cons1 all1 starts1 moves1)
	b =
    let
	m = 1 + maximum (0 :  setToList all1)
	TNFA cons2 all2 starts2 moves2 = mapTNFA opts (\ n -> n + m) b

	-- all that can be constructed from the start
	startmoves2 = starts2 `bind` (lookupset moves2)

	change t = if stcon t == tc then startmoves2 else unitSet t

	moves3 = mapFM (\ v ts -> ts `bind` change) moves1
	
	cons = (cons1 `minusSet` unitSet tc) `unionSet` cons2 
	all = all1 `unionSet` all2
	moves = plusFM_C (error "timesTNFA.moves") moves3 moves2

	c = TNFA cons all starts1 moves
    in	

	trinfo opts "times" c $
	c


timesTNFApublic :: Opts -> [TNFA Int] -> TNFA Int
timesTNFApublic opts args =
    if length args /= 3 
    then error "timesTNFApublic.args"
    else 
	let [tcarg, arg1, arg2] = args
	in  timesTNFA opts (cheat tcarg) arg1 arg2
