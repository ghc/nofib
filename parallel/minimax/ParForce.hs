{- -------------------------------------------------------------------------
$Id: ParForce.hs,v 1.2 1996/07/25 21:20:41 partain Exp $
Time-stamp: <Sat Apr 27 1996 22:58:48 Stardate: [-31]7419.57 hwloidl>

This is revision: $Revision: 1.2 $

Module for forcing parallelism using par and friends.

It contains three classes of functions:
1) For each _<par-annotation>_ that's hard-wired into the compiler, there is
   a function <par-annotation> in this module, doing the same thing as the 
   annotation but taking boxed values in the annotation positions. 
2) Forcing functions like forcelist that cause the evaluation of every 
   component of the data-structure. The clean way to handle this would be 
   to define a class Forcable with a force method for each datatype of 
   interest.
3) Parallel versions of some higher-order functions such as map, filter, zip 
   etc. Currently par_map is the most often used way to parallelise a program.
   The different versions force the argument list in different ways, and have 
   different ways of expressing the annotation information.

Currently two PP variables are used to decide which versions of the functions 
to take:
 SEQ ... creates dummy definitions for all annotations and makes the parallel
         higher order functions to aliases of their sequential counterparts
 GRAN_TNG ... uses the versions of the annotations that provide 
	        sparkname, gran-info, size-info, parallelism-info
              different versions of the parallel h.o. fcts pass this info in
              different ways
              [in the current versions the h.o. fcts just insert dummy values 
               for the size and parallelism info]

If no PP variable is defined, the parallel version is used, which only 
supplies spark names but no gran-, size- or par-info.

Changelog (including older versions not in the correwsponding RCS file):
$Log: ParForce.hs,v $
Revision 1.2  1996/07/25 21:20:41  partain
Bulk of final changes for 2.01

--# Revision 1.3  1994/11/23  01:07:23  hwloidl
--# Version for testing fwd mapping and hom sol.
--#
--# Revision 1.1  1994/11/19  02:00:05  hwloidl
--# Initial revision
--#
----------------------------------------------------------------------  -}

module ParForce (parGlobal, parLocal, parAt, parAtForNow, seq, par,
		 forcelist, forcelist1,  
		 parmap, parmap0, parmap1, pariterate,
		 parzip, parzipWith, parfilter,
		 par_map, par_map0, par_map1, par_iterate,
		 par_zip, par_zipWith, par_filter		 ) where

-- import Parallel
-- Basic parallelism annotations

#if defined(SEQ)

parGlobal :: Int -> Int -> Int -> Int -> a -> b -> b
parGlobal _ _ _ _ x y = y

parLocal :: Int -> Int -> Int -> Int -> a -> b -> b
parLocal _ _ _ _ x y = y

parAt :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAt _ _ _ _ w x y = y

parAtForNow :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAtForNow _ _ _ _ w x y = y

seq :: a -> b -> b
seq x y = y

par :: a -> b -> b
par x y = y

#elif defined(GRAN)

parGlobal :: Int -> Int -> Int -> Int -> a -> b -> b
parGlobal (I# n) (I# g) (I# s) (I# p) = _parGlobal_ n g s p
{-
	            where n' = case (n) of (I# n') -> n'
		          g' = case (g) of (I# g') -> g'
		          s' = case (s) of (I# s') -> s'
		          p' = case (p) of (I# p') -> p'
-}

parLocal :: Int -> Int -> Int -> Int -> a -> b -> b
parLocal (I# n) (I# g) (I# s) (I# p) = _parLocal_ n g s p

parAt :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAt (I# n) (I# g) (I# s) (I# p) = _parAt_ n g s p

parAtForNow :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAtForNow (I# n) (I# g) (I# s) (I# p) = _parAtForNow_ n g s p

{-# INLINE seq #-}
seq :: a -> b -> b
seq = _seq_

{-# INLINE par #-}
par :: a -> b -> b
par = _par_

#elif defined(PAR)  /* i.e.GUM */

import Parallel       -- includes definitions of par and seq

parGlobal :: Int -> Int -> Int -> Int -> a -> b -> b
parGlobal _ _ _ _ = par

parLocal :: Int -> Int -> Int -> Int -> a -> b -> b
parLocal _ _ _ _ = par

--parAt :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAt _ _ _ _ w = par

--parAtForNow :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAtForNow _ _ _ _ w = par

#else  /* e.g. HBCPP */

import Parallel -- renaming (par to hbcpp_par, seq to hbcpp_seq)

{-
--seq :: a -> b -> b
seq = hbcpp_seq

--par :: a -> b -> b
par = hbcpp_par "par"

--parGlobal :: Int -> Int -> Int -> Int -> a -> b -> b
parGlobal n _ _ _ = hbcpp_par (show n)

--parLocal :: Int -> Int -> Int -> Int -> a -> b -> b
parLocal n _ _ _ = hbcpp_par (show n)

--parAt :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAt n _ _ _ w = hbcpp_par (show n)

--parAtForNow :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAtForNow n _ _ _ w = hbcpp_par (show n)
-}

--parGlobal :: Int -> Int -> Int -> Int -> a -> b -> b
parGlobal n _ _ _ x y = par x y

--parLocal :: Int -> Int -> Int -> Int -> a -> b -> b
parLocal n _ _ _ x y = par x y

--parAt :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAt n _ _ _ w x y = par x y

--parAtForNow :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAtForNow n _ _ _ w x y = par x y

#endif 

forcelist [] = ()
forcelist (x:xs) = seq x (forcelist xs)

forcelist1 0 (x:xs) = ()
forcelist1 n (x:xs) = seq x (forcelist1 (n-1) xs)

#if 0 /* defined(SEQ) */

par_map :: Int -> (a -> b) -> [a] -> [b]
par_map _ = map

par_map0 :: Int -> (a -> b) -> [a] -> [b]
par_map0 _ = map

par_map1 :: (Int -> Int) -> (a -> b) -> [a] -> [b]
par_map1 _ = map

par_iterate :: Int -> (a -> a) -> a -> [a]
par_iterate _ = iterate

par_zipWith :: Int -> (a -> b -> c) -> [a] -> [b] -> [c]
par_zipWith _ = zipWith

par_zip :: Int -> [a] -> [b] -> [(a,b)]
par_zip _ = zip

par_filter :: Int -> (a -> Bool) -> [a] -> [a]
par_filter _ = filter

#else  {- !SEQ -}

parmap0 :: Int -> Int -> Int -> (a -> b) -> [a] -> [b]
parmap0 g s p  f []     = []
parmap0 g s p  f (x:xs) = parGlobal g g s p fx 
				( parGlobal g g s p (forcelist pmxs)  
						    (fx:pmxs) )
                  where fx = f x
			pmxs = parmap0 g s p  f xs
		  

par_map0 :: Int -> (a -> b) -> [a] -> [b]
par_map0 g = parmap0 g 0 0

parmap :: Int -> Int -> Int -> (a -> b) -> [a] -> [b]
parmap g s p  f []     = []
parmap g s p  f (x:xs) = parGlobal g g s p fx 
				( parGlobal g g s p (forcelist pmxs)  
						    (fx:pmxs) )
                  where fx = f x
			pmxs = parmap g s p f xs

par_map :: Int -> (a -> b) -> [a] -> [b]
par_map g = parmap g 0 0 
		  
parmap1 :: (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> (a -> b) -> [a] -> [b]
parmap1 g s p f l = parmap' 0 l
	            where parmap' n []     = []
		          parmap' n (x:xs) = parGlobal gn gn sn pn
						       pmxs 
						       (parGlobal gn gn sn pn
							          fx
				    			          (fx:pmxs) )
				    where fx = f x
					  pmxs = parmap' (n+1) xs
					  gn = g n
					  sn = s n
					  pn = p n

par_map1 :: (Int -> Int) -> (a -> b) -> [a] -> [b]
par_map1 g = parmap1 g ignore ignore
             where ignore _ = 0	 

pariterate :: Int -> Int -> Int -> (a -> a) -> a -> [a]
pariterate g s p  f x = parGlobal g g s p fx 
			{- parGlobal 14# 0# (forcelist rest) -}
					    (fx : rest)
		  where fx = f x
			rest = pariterate g s p f (f x)

par_iterate :: Int -> (a -> a) -> a -> [a]
par_iterate g = pariterate g 0 0

parzipWith :: Int -> Int -> Int -> (a -> b -> c) -> [a] -> [b] -> [c]
parzipWith g s p  z (a:as) (b:bs) = parGlobal g g s p zab  
				    (parGlobal g g s p  (forcelist rest) 
				   		        (zab : rest)
				    )
			      where zab = z a b
				    rest = parzipWith g s p  z as bs
parzipWith _ _ _ _ _ _ = []

par_zipWith :: Int -> (a -> b -> c) -> [a] -> [b] -> [c]
par_zipWith g = parzipWith g 0 0 

parzip :: Int -> Int -> Int -> [a] -> [b] -> [(a,b)]
parzip g s p = parzipWith g s p  (\ a b -> (a,b))

par_zip :: Int -> [a] -> [b] -> [(a,b)]
par_zip g = parzip g 0 0

-- This version is taken from Roe's thesis (p. 38)
parfilter :: Int -> Int -> Int -> (a -> Bool) -> [a] -> [a]
parfilter _ _ _  pred [] = []
parfilter g s p  pred (x:xs) = parGlobal g g s p  rest l
			   where rest = parfilter g s p  pred xs
			         l = if pred x then x:rest
					       else rest

par_filter :: Int -> (a -> Bool) -> [a] -> [a]
par_filter g = parfilter g 0 0

#endif  {- SEQ -}

-- ---------------------------------------------------------------------------
-- General way how to force data-stuctures
-- ToDo: Use strategies
-- ---------------------------------------------------------------------------

#if 0

class Forcable a where
 force :: a -> ()

instance Forcable [a] where
 force = forcelist	 

#endif 

