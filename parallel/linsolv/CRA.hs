{-
$Id: CRA.hs,v 1.1 1996/01/08 20:07:34 partain Exp $

This is revision: $Revision: 1.1 $

Module realizing the Chinese Remainder Algorithm (on 2 inputs as well as
on 2 input lists).

Changelog:
$Log: CRA.hs,v $
Revision 1.1  1996/01/08 20:07:34  partain
Initial revision

--# Revision 1.2  1994/12/13  19:53:03  hwloidl
--# tree_IMCRA0 is a tree-based version of CRA over lists.
--# Nomally it works over infinite lists (with -DNO_FILTER over finite lists;
--# NOTE: Depending on NO_FILTER different tree_IMCRA0 functions are compiled
--#       with *different typings*. So, any module using tree_IMCRA0 must be
--#       recompiled, too, when changing this flag).
--# s2IMCRA is the same as s1IMCRA (i.e. fold-based) but works on infinite lists.
--# tree_IMCRA0 handles the det-check now (therefore list of dets as input).
--# It tries to push the det-check as much backwards as possible by using the
--# handle fails fct (possible problem: only fails is immediately needed when calling
--# handle_fails)
--#
--# Revision 1.1  1994/12/08  19:24:21  hwloidl
--# Initial revision
--#
---------------------------------------------------------------------------- -}

module CRA (binCRA,s1IMCRA,s2IMCRA,tree_IMCRA0,
	    isPrime) where 

import ModArithm (modHom, modDif, modProd, modInv) 

import Random  (randomInts)    -- Just for testing
import ParForce 

#ifdef SEQ

par :: a -> b -> b
par x y = y

seq :: a -> b -> b
seq x y = y

_parGlobal_ :: Int# -> a -> b -> b
_parGlobal_ n x y = y

_parLocal_ :: Int# -> a -> b -> b
_parLocal_ n x y = y
#endif

#ifdef GUM
-- GUM has only par 

import {-fool mkdependHS; ToDo: rm-}
	Parallel

_parGlobal_ :: Int# -> a -> b -> b
_parGlobal_ n x y = par x y

_parLocal_ :: Int# -> a -> b -> b
_parLocal_ n x y = par x y
#endif

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n 
 | even n    = False
 | otherwise = isPrime' n 3

isPrime' :: Integer -> Integer -> Bool

isPrime' n l1     | n < l1*l1        = True
                  | n `mod` l1 == 0  = False
                  | otherwise        = isPrime' n (l1+2)


par_binCRA :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer 

par_binCRA m1 m2 inv a1 a2 = 
			 (if d == 0 then a1
                                    else a)
                         where ab = modHom m2 a1
                               d  = _parGlobal_ 0# a
					modDif m2 a2 a1
                               b  = modProd m2 d inv
                               b0 = if (b+b>m2) then b-m2
                                                    else b
                               a  = m1*b0+a1             {- not HWL version -}


binCRA :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer 

binCRA m1 m2 inv a1 a2 = (if d == 0 then a1
                                    else a)
                         where ab = modHom m2 a1
                               d  = modDif m2 a2 a1
                               b  = modProd m2 d inv
                               b0 = if (b+b>m2) then b-m2
                                                else b
                               a  =  m1*b0+a1             {- not HWL version -}

s1IMCRA :: [Integer] -> [Integer] -> (Integer,Integer)

s1IMCRA (m:ms) (r:rs) = foldl 
                        (\ (m0,r0) (m1,r1) -> (m0*m1,binCRA m0 m1 (modInv m1 m0) r0 r1) )
                        (m,r) (zip ms rs)

-----------------------------------------------------------------------------
-- Same as s1IMCRA but applicable for infinite lists
-----------------------------------------------------------------------------

s2IMCRA :: Integer -> [Integer] -> [Integer] -> [Integer] -> (Integer,Integer)
s2IMCRA mBound (m:ms) (r:rs) (d:ds)  = s2IMCRA' mBound ms rs ds m r 

s2IMCRA' :: Integer -> [Integer] -> [Integer] -> [Integer] -> Integer -> Integer ->
	    (Integer,Integer)
s2IMCRA' mBound (m:ms) (r:rs) (d:ds) accM accR 
  | accM > mBound = (accM, accR)
  | d == 0     = s2IMCRA' mBound ms rs ds accM accR
  | otherwise  = s2IMCRA' mBound ms rs ds (m*accM) (binCRA accM m (modInv m accM) accR r)

-----------------------------------------------------------------------------
-- Note:
--  The CRA on lists must be able to
--   - work on infinite lists (i.e. computing length on them is a *bad* idea)
--   - determine whether the prime in the m-list is lucky
--     that decision must be deferred as long as possible to avoid unnecessary
--     synchronisation; for that a list of det's in the  hom. im.s is
--     provided, too.
-----------------------------------------------------------------------------

-- Note: ms as ds are infinte lists 

tree_IMCRA0 :: Integer -> [Integer] -> [Integer] -> [Integer] -> 
		(Integer,Integer)


tree_IMCRA0 n ms as ds = 
	let
	  res@(m, a, fails) = {-_parGlobal_ 11# (forcelist ms')
				(_parGlobal_ 12# (forcelist as')
				  (_parGlobal_ 13# (forcelist ds') -}
			             tree_IMCRA0' ms' as' ds' -- ))
			      where ms' = take n ms 
				    as' = take n as 
				    ds' = take n ds
	  handle_fails n m a (m1:ms) (a1:as) (d1:ds) =
	     -- _parGlobal_ 46# m $
	     -- _parGlobal_ 47# a $
	      		 if n == 0 then (m, a)
	      		 else if d1 == 0 then (handle_fails n m a ms as ds)
	      		 else handle_fails (n-1) m' a' ms as ds
		   	      where
		         		m'  = m * m1
		         		a'  = par_binCRA m m1 inv a a1
		         		inv = modInv m1 m
	in
	  handle_fails fails m a ms as ds

tree_IMCRA0' [m] [a] [0] = (1, 1, 1)   -- FAIL due to unlucky prime
tree_IMCRA0' [m] [a] [_] = (m, a, 0)   -- normal case
tree_IMCRA0' ms as ds =
	let 
	  n = length ms
	  (left_ms, right_ms) = splitAt (n `div` 2) ms
	  (left_as, right_as) = splitAt (n `div` 2) as
	  (left_ds, right_ds) = splitAt (n `div` 2) ds
	  left@(left_M, left_CRA, left_fails)  = 
		tree_IMCRA0' left_ms left_as left_ds
	  right@(right_M, right_CRA, right_fails)  = 
		tree_IMCRA0' right_ms right_as right_ds
	  inv = modInv right_M left_M
	  cra = par_binCRA left_M right_M inv left_CRA right_CRA
	in
#if defined(TU_CRA)
	  _parGlobal_ 5# left
	   (_parGlobal_ 6# right
	     (_parLocal_ 9#  inv
	     --seq cra
  		(left_M * right_M, 
		 cra,
		 left_fails + right_fails ) -- ))
#elif defined(COMP_CRA)
	  _parGlobal_ 5# left_CRA
	   (_parGlobal_ 6# left_M
	    (_parGlobal_ 7#  right_CRA
	      (_parGlobal_ 8#  right_M
		(_parGlobal_ 9#  inv
  		  (left_M * right_M, 
		   cra,
		   left_fails + right_fails)))))
#elif defined(ASYM_CRA)
	  _parGlobal_ 5# left
	   (_parGlobal_ 7# right
	     (_parGlobal_ 9#  inv
		(seq cra
  		  (left_M * right_M, 
		   cra,
		   left_fails + right_fails)) ) )
#endif

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Experimental pat starts here
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- This is a brain damaged version of doing a binary-tree CRA over lists.

tree_IMCRA :: [Integer] -> [Integer] -> (Integer,Integer)

tree_IMCRA ms as = _parGlobal_ 0# inv 
				  (m1*m2, par_binCRA m1 m2 inv a1 a2)
		   where --( [(m1,a1),(m2,a2)] : _ ) =
			 --(m1,a1) = (head foo) !! 0
			 --(m2,a2) = (head foo) !! 1
			 -- foo :: Integer -- (Integer,Integer) -> (Integer,Integer) -> Boolean
			 [(m1, a1), (m2, a2)] =   
				head (dropWhile (\ l -> (length l) /= 2)
                             		  (iterate tree_IMCRA' (zip ms as)))

			 inv = modInv m2 m1
			 -- cra = par_binCRA m1 m2 inv a1 a2

tree_IMCRA' :: [(Integer,Integer)] -> [(Integer,Integer)]

tree_IMCRA' [] = []
tree_IMCRA' [(m1,a1)] = [(m1,a1)]
tree_IMCRA' ((m1,a1) : (m2,a2) : rest) = 
   _parGlobal_ 11# cra ({-_parGlobal_ 12# (forcelist rest')-} (cra : rest') )
   where cra = (m1*m2, binCRA m1 m2 (modInv m2 m1) a1 a2)
	 rest' = tree_IMCRA' rest

-- IMCRA with list of products of the modules as additional argument i.e.
-- in s2IMCRA m M r the following holds M_i = m_1 * ... * m_{i-1}

foldl' :: (a -> b -> c -> a) -> a -> [b] -> [c] -> a
foldl' f z [] _ = z
foldl' f z (x:xs) (l:ls) = foldl' f (f z x l) xs ls

--s2IMCRA :: [Integer] -> [Integer] -> [Integer] -> (Integer,Integer)

--s2IMCRA (m:ms) pp (r:rs) = foldl' 
--                        (\ (m0,r0) (m1,r1) -> (p,binCRA m0 m1 (modInv m1 m0) r0 r1) )
--                        (m,r) (zip ms rs) pp

s3IMCRA :: [Integer] -> [Integer] ->[Integer] -> (Integer,Integer)

s3IMCRA (m:ms) pp (r:rs) = foldl 
                        (\ (m0,r0) (m1,r1,p) -> (p,binCRA m0 m1 (modInv m1 m0) r0 r1) )
                        (m,r) (zip3 ms rs pp)

-----------------------------------------------------------------------------
-- Old cheating version
-----------------------------------------------------------------------------

#if 0   /* defined(NO_FILTER) */

-- Note: ms as are finte lists 
--	 no ds needed as no test (== 0) is performed

tree_IMCRA0 :: [Integer] -> [Integer] -> 
		(Integer,Integer)

-- tree_IMCRA0 [] [] = (1, 1)
tree_IMCRA0 [m] [a] = (m, a)
tree_IMCRA0 ms as =
	let 
		n = length ms
		(left_ms, right_ms) = splitAt (n `div` 2) ms
		(left_as, right_as) = splitAt (n `div` 2) as
		left@(left_M, left_CRA)  = tree_IMCRA0 left_ms left_as
		right@(right_M, right_CRA)  = tree_IMCRA0 right_ms right_as
		inv = modInv right_M left_M
	in
#if defined(TU_CRA)
		_parGlobal_ 5# left
		 (_parGlobal_ 6# right
		    (_parLocal_ 9#  inv
  		   	(left_M * right_M, 
			 par_binCRA left_M right_M inv left_CRA right_CRA)))
#elif defined(COMP_CRA)
		_parGlobal_ 5# left_CRA
		 (_parGlobal_ 6# left_M
		  (_parGlobal_ 7#  right_CRA
		   (_parGlobal_ 8#  right_M
		    (_parGlobal_ 9#  inv
  		   	(left_M * right_M, 
			 par_binCRA left_M right_M inv left_CRA right_CRA)))))
#elif defined(ASYM_CRA)
		_parGlobal_ 5# left
		 --(_parLocal_ 6# right
		    (_parGlobal_ 9#  inv
  		   	(left_M * right_M, 
			 par_binCRA left_M right_M inv left_CRA right_CRA)) --)
#endif	
        
{-
				--(left_CRA + right_CRA, 
				--binCRA left_M right_M inv left_CRA right_CRA) )
-}

{- Test check for equal length lists. 
	| length ms /= length as = error "tree_IMCRA0: different lengths of input lists"
-}

#endif

-- Old fail handler
	  {-
	  handle_fails 0 m a _       _       _      = (m, a)
	  handle_fails n m a (m1:ms) (a1:as) (0:ds) = 
		handle_fails n m a ms as ds
	  handle_fails n m a (m1:ms) (a1:as) (d1:ds) =
		handle_fails (n-1) m' a' ms as ds
		where
		  m'  = m * m1
		  a'  = par_binCRA m m1 inv a a1
		  inv = modInv m1 m

	  -}
