{- 
$Id: ParForce.hs,v 1.1 1996/01/08 20:07:35 partain Exp $

This is revision: $Revision: 1.1 $

  Module for forcing parallelism using par and friends.

Changelog:
$Log: ParForce.hs,v $
Revision 1.1  1996/01/08 20:07:35  partain
Initial revision

--# Revision 1.3  1994/11/23  01:07:23  hwloidl
--# Version for testing fwd mapping and hom sol.
--#
--# Revision 1.2  1994/11/19  21:50:18  hwloidl
--# *** empty log message ***
--#
--# Revision 1.1  1994/11/19  02:00:05  hwloidl
--# Initial revision
--#
--# Revision 1.1  1994/11/19  02:00:05  hwloidl
--# Initial revision
--#
----------------------------------------------------------------------  -}

module ParForce (parmap, parmap1, forcelist, forcelist1, par_iterate,
		 par_zip, par_zipWith) where

#ifdef SEQ

par :: a -> b -> b
par x y = y

seq :: a -> b -> b
seq x y = y

_parGlobal_ :: Int# -> a -> b -> b
_parGlobal_ n x y = y

#else

import {-fool mkdependHS; ToDo: rm-}
	Parallel

#endif

#ifdef GUM
-- GUM has only par 
_parGlobal_ :: Int# -> a -> b -> b
_parGlobal_ n x y = par x y

_parLocal_ :: Int# -> a -> b -> b
_parLocal_ n x y = par x y
#endif

forcelist [] = ()
forcelist (x:xs) = seq x (forcelist xs)

forcelist1 0 (x:xs) = ()
forcelist1 n (x:xs) = seq x (forcelist1 (n-1) xs)

parmap f []     = []
parmap f (x:xs) = _parGlobal_ 1# fx 
				( _parGlobal_ 6# (forcelist pmxs)  
						 (fx:pmxs) )
                  where fx = f x
			pmxs = parmap f xs
		  

parmap1 f l = parmap' l
	      where parmap' []     = []
		    parmap' (x:xs) = _parGlobal_ 1# 
						pmxs 
						(_parGlobal_ 2# 
							     fx
				    			     (fx:pmxs) )
				    where fx = f x
					  pmxs = parmap' xs


par_iterate :: (a -> a) -> a -> [a]
par_iterate f x = _parGlobal_ 13# fx 
			{- _parGlobal_ 14# (forcelist rest) -}
					(fx : rest)
		  where fx = f x
			rest = par_iterate f (f x)

par_zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
par_zipWith z (a:as) (b:bs) = _parGlobal_ 15# zab  
				(_parGlobal_ 16# (forcelist rest) 
				   		 (zab : rest)
				)
			      where zab = z a b
				    rest = par_zipWith z as bs
par_zipWith _ _ _ = []

par_zip :: [a] -> [b] -> [(a,b)]
par_zip = par_zipWith (\ a b -> (a,b))
