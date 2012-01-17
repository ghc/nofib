-- term algebras

module TA

( TCon, TCons, tconname, tconarity
, STerm	-- todo: do we need (..) to export instances with hbc?
, stcon, stargs, starity, mksterm

, var2id, var2exp, sterm2exp
)

where


import Set	-- syslib ghc

import Ids
import Syntax



type TCon = Id

tconname = idname
tconarity = idarity

type TCons = Set TCon


data STerm a = STerm TCon [a]
	deriving (Eq, Ord, Show)

-- hbc complains

{- # SPECIALIZE instance Eq (STerm Int) #-}
{- # SPECIALIZE instance Ord (STerm Int) #-}

{- # SPECIALIZE instance Eq (STerm (Int, Int)) #-}
{- # SPECIALIZE instance Ord (STerm (Int, Int)) #-}


stcon (STerm tcon _) = tcon
starity t = tconarity (stcon t)
stargs (STerm _ args) = args
mksterm tcon args = STerm tcon args

-------------------------------------------------------------------

var2id n = uservar ("x" ++ show n)
var2exp n = App (var2id n) []

sterm2exp t =

    let	tc = stcon t
	vs = map var2exp (stargs t)
    in
	-- looks like a function
	App tc vs



--------------------------------------------------------------------

