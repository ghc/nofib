module Gram2FA

( gram

)

where


import Maybes

import Set		-- syslib ghc
import FiniteMap	-- syslib ghc

import State

import Options

import Grammar

import Ids
import Syntax

import Semantik

import TA
import FAtypes
import FA

import FAmap
import FAcmpct
import FAconv

import Exp2FA


mapL             :: Monad m => (a -> m b) -> ([a] -> m [b])
mapL f []         = return []
mapL f (x:xs)     = do y<-f x; ys<-mapL f xs; return (y:ys)


-- converts an Exp describing a grammar into an ETNFA


gram :: Opts -> Env (Auto) (Auto) -> [Exp] -> FIO (Auto, Env (Auto) (Auto))
gram opts env xs =
    do	{ moops (length xs /= 2)
		( "grammar needs exactly two arguments "
		  ++ "(start expression and rule set): " ++ show xs )
	; let [ s, r ] = xs

	; moops (not (isColl r) || cType r /= CSet) 
		( "grammar rules must be given as set: " ++ show r)
	; let rs = cArgs r

	; rss <- mapL convertrule rs
	; let b = mkgram opts env s rss

	; 
--	  trace ("\ngram.xs = " ++ show xs) $
--	  trace ("\ngram.rss = " ++ show rss) $
--	  trace ("\ngram.b = " ++ show b) $
	  return (b, env)
	}



convertrule r =
    do	{ moops (not (isApp r)) 
		("rule in grammar must use (->): " ++ show r)
	; let App id xs = r
	; moops (idname id /= "->")
		("rule in grammar must use (->): " ++ show r)
	; moops (length xs /= 2)
		("(->) needs exactly two arguments: " ++ show r)
	; let [lhs, rhs] = xs
	; moops (not (isAppId lhs))
		("left hand arg of (->) must be identifier: " ++ show r)
	; let lhsname = idname (unAppId lhs)
	; return (lhsname, rhs)
	}

-------------------------------------------------------------------------

mkgram :: Opts -> Env (Auto) (Auto) -> Exp -> [(String, Exp)] -> Auto
mkgram opts e x rs = 
    let	vs = [ i | (i, _) <- rs ] 	-- local variables
	e' = delListFromFM e vs		-- they shadow global ones
    	(start, rules) = dosym (mkgs opts e' (mkSet vs) x rs)
	d = g2t opts (start, rules)
    in	

--	trace ("\nmkgram.vs = " ++ show vs) $
--	trace ("\nmkgram.start = " ++ show start) $
--	trace ("\nmkgram.rules = " ++ show rules) $
--	trace ("\nmkgram.d = " ++ show d) $

	d
	
------------------------------------------------------------------------


type MK a = Sym (Int, [(String, Either String (STerm String))]) a



--tnfa2grammar :: Ord a => Opts -> String -> TNFA a -> MK ()
tnfa2grammar opts name b @ (TNFA consb allb startsb movesb) =

--  trace ("\ntnfa2grammar.b : " ++ show b) $

    do	{ n <- mapL (\ a -> gensym >>= \ k -> return (a, k)) (setToList allb)
	; let h = listToFM n	
	; let c @ (TNFA consc allc startsc movesc) = 
		mapTNFA opts (lookupWithDefaultFM h (error "tnfa2grammar.c")) b
	; sequence [ push (v, Right t)   
		| (v, ts) <- fmToList movesc, t <- setToList ts ]
	; sequence [ push (name, Left s) | s <- setToList startsc ]
	}

--------------------------------------------------------------------------
 
mkgs :: Opts -> Env (Auto) (Auto) -> Set String -> Exp -> [(String, Exp)]
	-> MK String
mkgs opts env vars x rs =
    do	{ sequence (map (mkg opts env vars) rs)
	; start <- gensym
	; mkg opts env vars (start, x)
	; return start
	}

mkg :: Opts -> Env (Auto) (Auto) -> Set String -> (String, Exp) -> MK ()

mkg opts env vars (name, exp) 
  | isEmptySet (mkSet (map idname (appids exp)) `intersectSet` vars) =
    do	{ let (val, _) = forceFIO (comp opts env exp)
	; tnfa2grammar opts name val
	}

mkg opts env vars (name, App id []) = 
	-- must be a variable of the grammar
    push (name, Left (idname id))

mkg opts env vars (name, App id xs) | idname id == "++" =
    sequence 	[ do	{ nx <- gensym
			; push (name, Left nx) 
			; mkg opts env vars (nx, x)
			}
		| x <- xs ]

mkg opts env vars (name, x @ (App id xs)) = 
	-- a constructor (good)
	-- or a function call (bad)
    mkgname opts env vars name x id xs


mkg opts env vars (name, x) = 
    error ("cannot handle rule: " ++ show x)


mkgname opts env vars name x id xs =
    if exists (lookupFM env (idname id))
    then error ("function calls cannot have grammar vars as args: " ++ show x)
    else -- it's a constructor
     do	{ args <- mapL ( \ x -> do 
		{ k <- gensym; mkg opts env vars (k, x); return k } ) xs
	; push (name, Right (mksterm id args))
	}

--------------------------------------------------------------------


g2t :: (Show a, Ord a) => Opts -> Grammar a -> TNFA Int
g2t opts = cmpctTNFA opts . etnfa2tnfa opts . grammar2etnfa opts

