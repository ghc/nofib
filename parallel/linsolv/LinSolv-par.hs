{-
$Id: LinSolv-par.hs,v 1.1 1996/01/08 20:07:34 partain Exp $

This is revision: $Revision: 1.1 $

Module for computing the solution of a system of linear system of equations
in several variables.
The rhs of the system of equations is the first parameter (given as a 
square matrix). The lhs of the system is the second parameter (given as a
vector).

ChangeLog:
$Log: LinSolv-par.hs,v $
Revision 1.1  1996/01/08 20:07:34  partain
Initial revision

--# Revision 1.4  1994/12/13  19:49:38  hwloidl
--# Version based on infinite lists.
--# Has seq steps in all variants (even without det check)
--# -DNOFILTER uses finite part of inifinte xList and avoids any det check
--# -DSEQ_CRA uses a fold-based CRA instead of the tree-based one (both over
--#           infinte lists)
--#
--# Revision 1.3  1994/11/23  01:04:43  hwloidl
--# Test version of fwd mapping and hom sol (no CRA in this version)
--#  New: prime list computed in par and packet forced
--#       modDet computed in par to hom sol
--#       No modDet == 0 test in gen_xList to avoid seq in that step ->
--#       count and filter are used to detect number of necessary hom ims
--#       xList is now again an infinite list (it is not forced in this version)
--#  Note: The result of linsolv just triggers computation up to hom sols but
--#        not until CRAs. The value is not important, just the tiggering
--#  NO_FILTER flag: if set any modDet == 0 test is avoided (i.e. no filtering
--#                  either; this doesn't always yield a correct solution but
--#                  shows how this test seq the computation
--#
--# Revision 1.2  1994/11/19  21:49:38  hwloidl
--# Compute list of primes in parallel to fwd mapping and sol in hom ims
--#
--# Revision 1.1  1994/11/19  02:00:05  hwloidl
--# Initial revision
--#

---------------------------------------------------------------------------- -}

module LinSolv(linSolv) where

import ModArithm {- (modHom, Hom(hom))-}

import Matrix (SqMatrix, Vector, vector, matBounds,
               determinant, transp, replaceColumn, size,
               maxElem, maxElemVec, scalarMult, vecScalarQuot,
               matGcd, vecGcd, matHom, vecHom) 

import CRA (s1IMCRA,s2IMCRA,tree_IMCRA0)
  
#ifndef SEQ

import ParForce 
import {-fool mkdependHS; ToDo: rm-}
	Parallel

#else

parmap = map

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
_parGlobal_ :: Int# -> a -> b -> b
_parGlobal_ n x y = par x y

_parLocal_ :: Int# -> a -> b -> b
_parLocal_ n x y = par x y
#endif

-- ---------------------------------------------------------------------------
-- Auxiliary functions
-- ---------------------------------------------------------------------------

primes :: [Integer]

primes = 2 : oddPrimesFrom 3
         where oddPrimesFrom n | isPrime n primes = n : oddPrimesFrom (n+2)
                               | otherwise        =     oddPrimesFrom (n+2)

isPrime :: Integer -> [Integer] -> Bool

isPrime n (l1:ls) | n < l1*l1        = True
                  | n `mod` l1 == 0  = False
                  | otherwise        = isPrime n ls

projection :: Integer -> [[Integer]] -> [Integer]

projection i [] = [] 
projection i (ls1:lss) = ls1!!(i-1) : projection i lss
 
fact :: Integer -> Integer

fact 0   = 1
fact (n+1) = (n+1) * fact n

-- ---------------------------------------------------------------------------
-- Function for solving a system of linear equations
-- ---------------------------------------------------------------------------

-- linSolv :: (Integral a) => SqMatrix a -> Vector a -> (a,a,Vector a)   

linSolv a b = 
  let 
   {- Step1: -}
    n = toInteger (size a) 
    s = max (maxElem a) (maxElemVec b)
    pBound = 2 * s^n * fact n

   {- Step2: -}
    xList :: [[Integer]]
    xList = _parGlobal_ 19# (forcelist1 100 primes) 
		_parGlobal_ 18# noOfPrimes 
		    (gen_xList primes)

    gen_xList :: [Integer] -> [[Integer]]
    gen_xList (p:ps)
		    = let 
			b0 = vecHom p b
			a0 = matHom p a
			modDet = modHom p (determinant a0)

			{-  Parallelism inside a homomorphic solution -}

			homSol = _parGlobal_ 8# modDet 
				                (p : modDet : pmx)

			pmx = parmap ( \ j ->
					let
                      				a1 = replaceColumn j a0 b0
                    		  	in
                      				modHom p (determinant a1) )
                  		     [jLo..jHi] 
			((iLo,jLo),(iHi,jHi)) = matBounds a

			restList = gen_xList ps 
		      in
			_parGlobal_ 3#  homSol 
					(homSol : restList)

   {- Step3: -}

   {- s1IMCRA variant -}

    count (l1:ls) i accum = if accum>pBound
                              then i
                              else count ls (i+1) (l1*accum)

    -- noOfPrimes = count (projection 1 xList) 0 1 

    -- Just for TESTING:
    --  noOfPrimes_0 = count primes 0 1 
    --  xList_0 = take noOfPrimes xList

#if defined(NO_FILTER)
    {-
    noOfPrimes = count (projection 1 xList) 0 1 
    xList'' = take noOfPrimes xList

    luckyPrimes :: [Integer]
    luckyPrimes = projection 1 xList''
    -}

    -- 1. The lists passed to CRA are finite
    -- 2. No test for det == 0 is ever performed

    noOfPrimes = count (projection 1 xList) 0 1

    primeList = take noOfPrimes (projection 1 xList)
    detList = take noOfPrimes (projection 2 xList)

    -- Force the lists?

    det = snd (tree_IMCRA0 primeList detList) 

    x_i i = snd (tree_IMCRA0 primeList x_i_List)
	    where
	          x_i_List = take noOfPrimes (projection (i+2) xList)

    x = vector (parmap x_i [1..n])

#else
    -- No more cheating ...

    {- The naive, inefficient version
    xList' = filter (\ (x1:x2:xs) -> x2 /= 0) xList
    noOfPrimes = count (projection 1 xList') 0 1
    xList'' = take noOfPrimes xList'
    -}

    noOfPrimes = count (projection 1 xList) 0 1
    primeList = projection 1 xList
    detList = projection 2 xList     -- list of dets in hom ims if det==0
				     -- we will throw the result away in 
				     -- tree_IMCRA0 => needed in there
#if defined(SEQ_CRA)
    det = a
	  where (_, a) = s2IMCRA pBound {-noOfPrimes-} primes detList detList

    x_i i = a
	    where (_, a) = s2IMCRA pBound {-noOfPrimes-} primes x_i_List detList
	          x_i_List = projection (i+2) xList
#else
    det = snd (tree_IMCRA0 noOfPrimes primeList detList detList)

    x_i i = snd (tree_IMCRA0 noOfPrimes primeList x_i_List detList)
	    where
	           x_i_List = projection (i+2) xList
#endif

    x = vector (parmap x_i [1..n])
#endif

   {- Step4: -}
    gcdVec = vecGcd x
    gcdVal = seq det 
		   (gcd gcdVec det)

    -- Solution i.e. a * (res_a/res_b) * res_x = b
    res_a :: Integer
    res_a = gcdVec `div` gcdVal
    res_b :: Integer
    res_b = det `div` gcdVal
    res_x :: Vector Integer
    res_x = vecScalarQuot gcdVec x
  in 
    -- (det, gcdVec, gcdVal, x)   -- Just for testing
    -- (pBound , length xList, last xList) --, sum (map sum xList))
    -- (noOfPrimes, pBound, sum (parmap sum xList)) --, head xList, last xList) --, sum (map sum xList))
    --(noOfPrimes, pBound, sum (parmap sum xList''), det, x)
    --(noOfPrimes, pBound, det)
    --(take 20 xList)
    --(res_x, res_a, res_b)
    (vecScalarQuot gcdVec x, gcdVec `div` gcdVal, det `div` gcdVal)   
    --(x, 99, 99)   
    --(noOfPrimes, x)

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   {- s3IMCRA variant:  

    --luckyPrimes :: [Integer]
    --luckyPrimes = take noOfPrimes (projection 1 xList)

    (noOfPrimes, luckyPrimes, primeProds) = count (projection 1 xList) 0 1 
        where count :: [Integer] -> Integer -> Integer -> (Integer,[Integer],[Integer])
              count (l1:ls) i accum =  
                    let 
                         new_accum = l1*accum
                         res@(n,lP,pP) = count ls (i+1) new_accum
                       in 
                         if accum>pBound
                           then (i, [], [])
                           else (n, l1:lP, new_accum:pP)

    det = snd( s3IMCRA luckyPrimes
                       (take noOfPrimes (projection 2 xList))
                       primeProds )

    x_i i = snd( s3IMCRA luckyPrimes
                         (take noOfPrimes (projection (i+2) xList))
                         primeProds )

    x = vector [ x_i i | i <- [1..n] ]              {- should be a vector -}
   -}

{-
main =
 let 
   a1 = sqMatrix (listArray ((1,1),(3,3)) [ 1, 1, 1, 
                                            0, 1, 2,
                                            2, 2, 1] )
   l1 = [6, 8, 9]
   b1 = vector l1
   x = linSolv a1 b1
 in
   appendChan stdout "Testing linSolv: \n" abort $
   appendChan stdout "Solving a1*x=b1 with the following a1 and b1: \n a1 = " abort $
   appendChan stdout (show a1) abort $
   appendChan stdout "\nb1 = " abort $
   appendChan stdout (show b1) abort $
   appendChan stdout "\nx = " abort $
   appendChan stdout (show x) abort $
   done 
-}
 
   {- Seq version: 
    xList :: [[Integer]]
    xList = [ let
                 b0 = vecHom p b
               in 
                  p : modDet :
                  [ let
                      a1 = replaceColumn j a0 b0
                    in
                      modHom p (determinant a1)
                  | j <- [jLo..jHi] ]
             | p <- primes, a0 <- [matHom p a], 
               modDet <- [modHom p (determinant a0)],
               modDet /= 0 ]
             where
               ((iLo,jLo),(iHi,jHi)) = matBounds a
   -}
 
{- 
  Old version of the body of gen_xList:
			if modDet == 0 
			  then restList
			  else   _parGlobal_ 3# homSol 
					(_parGlobal_ 4# (forcelist restList)
						        (homSol : restList) )

-}

			{-
			Seq. hom sol

			homSol = p : modDet :
                  		 [ let
                      			a1 = replaceColumn j a0 b0
                    		   in
                      			modHom p (determinant a1)
                  		 | j <- [jLo..jHi] ]
				 where ((iLo,jLo),(iHi,jHi)) = matBounds a
			-}
