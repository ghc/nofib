-- C-like imperative semantics:

-- expressions have values and side effects (that modify the environment)
-- and they may do output

module Semantik 

( Env

, FIO, unFIO, forceFIO

, oops
, moops

, Fun, mkFun, mkfunction

, vargs

, comp
, docomp

)

where

import Maybes

import Options

import FiniteMap -- syslib ghc

import Syntax
import Ids

import FAcon


-- identifiers are bound to functions from Exp^* -> a
-- that is they see the literal form of their arguments
-- they need to evaluate them if they want
-- this is like lisp and allows for (setq foo bar)

newtype FIO s = FIO (Either String s); unFIO (FIO n) = n

instance Functor FIO where
    fmap f (FIO (Left l)) = FIO (Left l)
    fmap f (FIO (Right r)) = FIO (Right (f r))

instance Monad FIO where
    return x = FIO (Right x)
    FIO (Left l) >>= f = FIO (Left l)
    FIO (Right r) >>= f = f r

-- instance MonadPlus FIO where
--    mzero = FIO (Left "some error")

oops :: String -> FIO a
oops cs = FIO (Left cs)

moops :: Bool -> String -> FIO ()
moops p cs = if p then oops cs else return ()

forceFIO :: FIO a -> a
forceFIO (FIO (Left l)) = error ("error (FIO): " ++ l)
forceFIO (FIO (Right r)) = r


-- only look at the result
docomp opts env arg = 
    forceFIO (do { (x, env') <- comp opts env arg; return x } )



-------------------------------------------------------------------

type Env e a = FiniteMap String (Fun e a)

data Fun e a = Fun (Opts -> Env e a -> [Exp] -> FIO (a, Env e a))
mkFun f = Fun f; unFun (Fun f) = f


--------------------------------------------------------------------

-- a plain function that evaluates its arguments

-- mkfunction :: String -> ([a] -> a) -> Fun e a
mkfunction name f = Fun (\ opts env args -> 

    do	{ troff opts ("\nentered: " ++ name) (return ())
	; (vals, env1) <- vargs opts env args
	; return (f opts vals, env1)	-- todo: really env1 here?
	} )


----------------------------------------------------------------------

-- evaluate a list of expressions from left to right
-- return list of results
-- thread state through

-- vargs :: Opts -> Env e a -> [Exp] -> FIO ([a], Env e a)
vargs opts env [] = return ([], env)
vargs opts env (x : xs) = 
    do	{ (y, env1) <- comp opts env x
	; (ys, env2) <- vargs opts env1 xs
	; return (y : ys, env2)
	}


-- a computation
-- has a result
-- maybe changes the environment
-- maybe does some FIO
-- sequential composition ";" and assignment "=" are wired in

-- comp :: Opts -> Env e a -> Exp -> FIO (a, Env e a)

comp opts env (App id args) | idname id == ";" =
    do	{ (xs, env1) <- vargs opts env args
	; return (last xs, env1)
	}

comp opts env x @ (App id args) | idname id == "=" =
    do	{ moops (length args /= 2)
		( "(=) needs exactly two arguments: " ++ show x )
	; let [lhs, rhs] = args

	; case lhs of
	    App id locs -> compbind opts env x (idname id) locs rhs
	    _ -> oops ( "lhs of (=) must be application of function or operator: " ++ show x )
	}

comp opts env x @ (App id args) =

    troff opts ("\ncomp: " ++ show x ) $

    let name = idname id in case lookupFM env name of
	Just f -> unFun f opts env args
	Nothing -> -- oops ("identifier " ++ name ++ " not bound")
		-- NO, rather: unbound ids are treated as constructors

		-- todo: this breaks the abstraction
	    do	{ (vs, env1) <- vargs opts env args
		; return (conTNFA opts id vs, env1)
		}


compbind opts env x name locs rhs = 
    do	{ moops (exists (lookupFM env name))
		( "identifier already bound: " ++ show x )
	
	; if null locs 
	  then define_value    opts env name rhs	-- see below
	  else define_function opts env x name locs rhs	-- see below
	}

-------------------------------------------------------------------

mkconst :: a -> Fun e a
mkconst x =  Fun ( \ opts env args -> do	
	{ moops (not (null args)) 
		("a constant cannot have args: " ++ show args)
	; return (x, env)
	} )

-- a value (function with 0 args) is evaluated right now
define_value opts env name rhs =
    do	{ (v, env1) <- comp opts env rhs -- env1 is ignored
	; let val = mkconst v
	; let env2 = addToFM env name val
	; return (v, env2)
	}

-- a `real' function (with > 0 args) is stored as closure
define_function opts env x name lhsargs rhs =
    do	{ moops (any (not . isAppId) lhsargs) 
		( "local args must be ids: " ++ show x )
	; let locs = map (idname . unAppId) lhsargs

	-- here's the semantics of a function call
	; let val = Fun (\ opts env1 args1 -> do	
		-- evaluate args in caller's environment
		{ (vs, env2) <- vargs opts env1 args1
		; moops (length vs /= length locs)
			( "wrong number of args: " ++ show args1
			  ++ ", should be " ++ show (length locs) )
		-- local bindings over callee's environment
		; let bnds = listToFM (zip locs (map mkconst vs))
		; let env3 = env1 `plusFM` bnds
		; (v, env4) <- comp opts env3 rhs
		-- return caller's environment
		; return (v, env2)
		} )

	; let env1 = addToFM env name val

--	; return (undefined, env1)	-- todo: what to return here?
	; return (conTNFA opts (usercon 0 "defined") [], env1)	

	}


