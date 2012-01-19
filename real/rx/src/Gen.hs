module Gen 

( genval
, genpid
, genenv
)

where

import FiniteMap
import Ids
import IdStack


import FA
import FAtypes 
import FAuseful
import FA2Exp

import Grammar
import Gram2FA
import Loop 
import Semantik

import WrapSubtrans

---------------------------------------------------------------------

genval :: Val (Env (Auto) (Auto))
genval opts env x = case unFIO (comp opts env x) of
    Right (x1, env1) -> Right (tnfa2exp opts x1, env1)
    Left msg -> Left msg


-- for parsing
genpid :: IdStack
genpid = globIS ( inIts (
		[ userfun 2 "grammar"	-- todo: wrong place
		, userfun 2 "subtrans"	-- todo: wrong place
		] 
		++
		[ id | (id, _) <- fids ] 
	))

-- for evaluating
genenv = listToFM  (
	[ ( idname id
	  , mkfunction (idname id) 
		(\ opts -> usefulTNFA opts . val opts) -- wrong place?
	  )
	| (id, val) <- fids 
	] 
	++
	[ ("grammar", mkFun gram) 
	, ("subtrans", mkFun subtrans) 
	]
		 )

