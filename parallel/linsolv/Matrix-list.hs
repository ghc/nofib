{- 
$Id: Matrix-list.hs,v 1.1 1996/01/08 20:07:34 partain Exp $

This is revision: $Revision: 1.1 $

Data Encapsulation of the ADT Matrix.
Internal representation is a list of lists.

Changelog:
$Log: Matrix-list.hs,v $
Revision 1.1  1996/01/08 20:07:34  partain
Initial revision

--# Revision 1.3  1994/11/23  01:06:18  hwloidl
--# Version for testing fwd mapping and hom sol.
--# Unchanged from previous version
--#
--# Revision 1.2  1994/11/19  21:50:15  hwloidl
--# *** empty log message ***
--#
--# Revision 1.1  1994/11/19  02:00:05  hwloidl
--# Initial revision
--#
--# Revision 1.1  1994/11/19  02:00:05  hwloidl
--# Initial revision
--#
--------------------------------------------------------------------------  -}

module Matrix(SqMatrix, Vector, {- MatBounds, -}
              listSqMatrix, lolSqMatrix, vector,
              determinant, transp, replaceColumn, size,
              maxElem, maxElemVec, scalarMult, vecScalarQuot, 
              matGcd, vecGcd, matHom, vecHom, matBounds,
	      matMult, jaH) 
              {- showsMatrix, matEqual, matSum, matMult)
                 matSum',matSum'',showIt,makeUnique) -}                   where

import ModArithm ({- Hom(hom), -} modHom)

#ifndef SEQ
import ParForce 
#endif

-- ----------------------------------------------------------------------------
-- Data Type definitions
-- ----------------------------------------------------------------------------

data  (Integral a)  =>  SqMatrix a = SqMatrixC MatBounds [[a]]
data  (Integral a)  =>  Vector a   = VectorC VecBounds [a]

type  MatBounds = ((Int,Int),(Int,Int))
type  VecBounds = ((Int),(Int))

-- ----------------------------------------------------------------------------

lol :: (Integral a) => Int -> [a] -> [[a]]

lol _ [] = []
lol n l = let 
	    (line, rest) = splitAt n l
          in
	    line : (lol n rest)

#ifndef SEQ

mat_map = parmap

listCompwiseComp :: (a -> b -> c) -> [a] -> [b] -> [c]

listCompwiseComp = par_zipWith 
			  		{- parmap f' (parzip l l')
			  		where f' (a,b) = a `f` b -}
#else

mat_map = map

listCompwiseComp :: (a -> b -> c) -> [a] -> [b] -> [c]

listCompwiseComp = zipWith 
			  		{- map f' (zip l l')
			  		where f' (a,b) = a `f` b -}
#endif

-- ----------------------------------------------------------------------------
-- Data type constructors
-- ----------------------------------------------------------------------------

{- ----------------------------------------------------------------------------
sqMatrix, i.e. arrays removed for GUM
   ----------------------------------------------------------------------------

sqMatrix :: (Integral a)  =>  Array (Int,Int) a -> SqMatrix a

sqMatrix arr = SqMatrixC b [ [ (arr!(i,j)) | j <- [jLo..jHi] ]
                           | i <- [iLo..iHi] ] 
	       where b@((iLo,jLo),(iHi,jHi)) = (bounds arr)
-}
listSqMatrix :: (Integral a)  =>  MatBounds -> [a] -> SqMatrix a

listSqMatrix b@((iLo,jLo),(iHi,jHi)) l = SqMatrixC b (take m (lol n l))
					 where m = iHi - iLo +1
					       n = jHi - jLo + 1

lolSqMatrix :: (Integral a)  =>  MatBounds -> [[a]] -> SqMatrix a

lolSqMatrix b l = SqMatrixC b l

matBounds (SqMatrixC b _) = b

vector :: (Integral a)  =>  [a] -> Vector a

vector l = VectorC ((1),(n)) l
           where n = length l

vecBounds (VectorC b _) = b

-- only for testing

jaH (VectorC b l) = l

-- ----------------------------------------------------------------------------
-- Mapping and other general operations
-- ----------------------------------------------------------------------------

matMapUnary :: (Integral a)  =>  
               (a -> a) -> SqMatrix a -> SqMatrix a

matMapUnary f (SqMatrixC b mat) =
	SqMatrixC b (mat_map (mat_map f) mat)

matCompwiseComp :: (Integral a, Integral b, Integral c)  =>  
                   (a -> b -> c) -> SqMatrix a -> SqMatrix b -> SqMatrix c

matCompwiseComp f (SqMatrixC bnds@((iLo,jLo),(iHi,jHi)) mat) (SqMatrixC bnds' mat') = 
       if (bnds==bnds')
         then SqMatrixC bnds [ listCompwiseComp f (mat!!(k-1)) (mat'!!(k-1))
			     | k <- [iLo..iHi] ]
         else error "matCompwiseComp: Matrices have different bounds\n"

matFold :: (Integral a)  =>  (a -> a -> a) -> a -> SqMatrix a -> a

matFold f init (SqMatrixC _ mat) = foldl f init (mat_map (foldl f init) mat)


vecFold :: (Integral a)  =>  (a -> a -> a) -> a -> Vector a -> a

vecFold f init (VectorC _ mat) = foldl f init mat 

-- ----------------------------------------------------------------------------
-- Misc operations
-- ----------------------------------------------------------------------------
 
size :: (Integral a)  =>  SqMatrix a -> Int

size (SqMatrixC ((iLo,jLo),(iHi,jHi)) mat) = 
			if (iLo==jLo) && (iHi==jHi)
                         then iHi-iLo+1
                         else error "size: Matrix doesn't have size ((1,1),(n,n))\n"


-- replaceColumn :: (Ix a, Ix b) => a -> Array (a,b) c -> Array b c -> Array (a,b) c

replaceColumn :: (Integral a) => Int -> SqMatrix a -> Vector a -> SqMatrix a

-- This is definitely more elegant. But is it as efficient?

replaceColumn j (SqMatrixC b m)(VectorC _ v) = 
		SqMatrixC b (transpose (replaceLine j v (transpose m)))
		where   replaceLine :: Int -> [a] -> [[a]] -> [[a]]
			replaceLine j v m = ( take (j-1) m ) ++
					  [v] ++
					  ( drop (j) m )
			
{-
replaceColumn j (SqMatrixC b@((iLo,jLo),(iHi,jHi)) mat) (VectorC _ v) = 
     if (not (inRange (jLo,jHi) j)) 
       then error "Error in replaceColumn: column index not in range"
       else SqMatrixC b [ replaceElem j i | i <- [iLo..iHi] ]
	    where replaceElem j i = [ line !! (k-1) | k <- [jLo..j-1] ] ++
				    [ v !! (i-1) ] ++
				    [ line !! (k-1) | k <- [j+1..jHi] ]
		  		    where line = mat !! (i-1)
-}

-- transp :: (Ix a, Ix b) => Array (a,b) c -> Array (b,a) c

transp :: (Integral a) => SqMatrix a -> SqMatrix a

transp (SqMatrixC b@((iLo,jLo),(iHi,jHi)) mat) = SqMatrixC b (transpose mat)
	{- 
	SqMatrixC b [ [ line !! (j-1) | line <- mat ] | j <- [jLo..jHi] ]
	-}

-- maxElem :: (Ix a, Ix b, Ord c) => Array (a,b) c -> c

maxElem :: (Integral a) => SqMatrix a -> a

maxElem (SqMatrixC _ mat) = maximum ( mat_map maximum mat )

maxElemVec :: (Integral a) => Vector a -> a

maxElemVec (VectorC _ vec) = maximum vec

-- ----------------------------------------------------------------------------
-- Arithmetic Operations
-- ----------------------------------------------------------------------------
 
-- scalarMult :: (Ix a, Ix b, Num c) => c -> Array (a,b) c -> Array (a,b) c

scalarMult :: (Integral a) => a -> SqMatrix a -> SqMatrix a

scalarMult x = matMapUnary (x*)
		{-
	      	SqMatrixC b [ mat_map (x*) line | line <- mat ]
		-}

vecScalarQuot :: (Integral a) => a -> Vector a -> Vector a

vecScalarQuot x (VectorC b vec) = 
              VectorC b (mat_map (`div` x) vec)

crossProd :: (Integral a) => Vector a -> Vector a -> a

crossProd (VectorC _ vec) (VectorC _ vec') =
	foldl (+) 0 (listCompwiseComp (*) vec vec')

-- determinant :: (Ix a, Ix b, Num c) => Array (a,b) c -> c

determinant :: (Integral a) => SqMatrix a -> a

determinant (SqMatrixC ((iLo,jLo),(iHi,jHi)) mat) 
	| jHi-jLo+1 == 1 =  let 
			      [[mat_1_1]] = mat 
			    in 
			      mat_1_1
	| jHi-jLo+1 == 2 =  let  
			      [[mat_1_1,mat_1_2],
			       [mat_2_1,mat_2_2] ] = mat
			    in
			      mat_1_1 * mat_2_2 -  mat_1_2 * mat_2_1
	| otherwise      =  
#ifndef SEQ
#ifdef PAR_DET_SZ
             sum (if (size<PAR_DET_SZ) then l_seq else l_par)
#else
	     sum l_par
#endif
#else
	     sum l_seq
#endif
             where
	       newLine _ [] = []
	       newLine j line = [ line !! (k-1) | k <- [jLo..j-1] ] ++
				[ line !! (k-1) | k <- [j+1..jHi] ]

               size = jHi-jLo+1

#ifndef SEQ
               l_par =  parmap 
		     ( \ j ->
               	      (let 
                        sign = if (even (j-jLo)) then 1
                                                 else -1
                        mat' = SqMatrixC ((iLo,jLo),(iHi-1,jHi-1))
				(map (newLine j) (tail mat))
                        x = sign * ( (head mat) !! (j-1) ) * (determinant mat')
                       in
                        x) )
                     [jLo..jHi] 
#endif

               l_seq =  [ let 
                        sign = if (even (j-jLo)) then 1
                                                 else -1
                        mat' = SqMatrixC ((iLo,jLo),(iHi-1,jHi-1))
				(map (newLine j) (tail mat))
                        x = sign * ( (head mat) !! (j-1) ) * (determinant mat')
                      in
                        x
                    |
                      j <- [jLo..jHi] ]

-- matEqual :: (Ix a, Ix b, Eq c) => Array (a,b) c -> Array (a,b) c -> Bool

matEqual :: (Integral a) => SqMatrix a -> SqMatrix a -> Bool

matEqual (SqMatrixC bnds@((iLo,jLo),(iHi,jHi)) mat) (SqMatrixC bnds' mat') = 
       if (bnds==bnds')
         then foldl (&&) True 
                    [ foldl (&&) True 
                            (listCompwiseComp (==) (mat !! (k-1)) (mat' !! (k-1)))
                    | k <- [iLo..iHi] ]
         else error "matEqual: Matrices have different bounds\n"


vecEqual :: (Integral a) => Vector a -> Vector a -> Bool

vecEqual (VectorC bnds vec) (VectorC bnds' vec') = 
       if (bnds==bnds')
         then foldl (&&) True (listCompwiseComp (==) vec vec')
         else error "vecEqual: Matrices have different bounds\n"

-- matSum :: (Ix a, Ix b, Num c) -> Array (a,b) c -> Array (a,b) c -> Array (a,b) c

matSum :: (Integral a) => SqMatrix a -> SqMatrix a -> SqMatrix a

matSum = matCompwiseComp (+)


matDif :: (Integral a) => SqMatrix a -> SqMatrix a -> SqMatrix a

matDif = matCompwiseComp (-)

{-
matSum :: (Num a) => SqMatrix a -> SqMatrix a -> SqMatrix a

matSum (SqMatrixC mat) (SqMatrixC mat') = 
       if (bnds==bnds')
         then SqMatrixC (array bnds
                         [ i := mat!i + mat'!i | i <- indices mat ] )
         else error "matSum: Matrices have different bounds\n"
       where bnds  = bounds mat
             bnds' = bounds mat'
             
-- matDif :: (Ix a, Ix b, Num c) -> Array (a,b) c -> Array (a,b) c -> Array (a,b) c

matDif :: (Num a) => SqMatrix a -> SqMatrix a -> SqMatrix a

matDif (SqMatrixC mat) (SqMatrixC mat') = 
       if (bnds==bnds')
         then SqMatrixC (array bnds
                         [ i := mat!i - mat'!i | i <- indices mat ] )
         else error "matDif: Matrices have different bounds\n"
       where bnds  = bounds mat
             bnds' = bounds mat'
-}

-- matMult :: (Ix a, Ix b, Ix c, Num d) => 
--            Array (a,b) d -> Array (b,c) d -> Array (a,c) d

-- matMult :: (Integral a) => SqMatrix a -> SqMatrix a -> SqMatrix a

matMult (SqMatrixC bnds mat) (SqMatrixC bnds' mat') = 
        SqMatrixC resultBounds 
	[ [ let 
		line =    (VectorC ((jLo),(jHi)) (getLine i mat))
		column =  (VectorC ((iLo'),(iHi')) (getColumn j mat'))
	    in 
		crossProd line column
          | j <- [jLo..jHi] ] 
        | i <- [iLo..iHi] ]
	where getLine i mat = mat !! (i-1)
	      getColumn j mat = [ line !! (j-1) | line <- mat ]
              ((iLo,jLo),(iHi,jHi)) = bnds
              ((iLo',jLo'),(iHi',jHi')) = bnds'
              resultBounds 
               | (jLo,jHi)==(iLo',iHi')  = ((iLo,jLo'),(iHi,jHi'))
               | otherwise               = error "matMult: incompatible bounds"


matAbs :: (Integral a) => SqMatrix a -> SqMatrix a

matAbs = matMapUnary abs


matSignum :: (Integral a) => SqMatrix a -> SqMatrix a

matSignum = matMapUnary signum


{-
matAbs :: (Num a) => SqMatrix a -> SqMatrix a

matAbs (SqMatrixC mat) = 
       SqMatrixC (array (bounds mat)
                  [ i := abs (mat!i) | i <- indices mat ] )

 
matSignum :: (Num a) => SqMatrix a -> SqMatrix a

matSignum (SqMatrixC mat) = 
          SqMatrixC (array (bounds mat)
                      [ i := signum (mat!i) | i <- indices mat ] )
-}

matGcd :: (Integral a)  =>  SqMatrix a -> a

matGcd m = matFold gcd (maxElem m) m


vecGcd :: (Integral a)  =>  Vector a -> a

vecGcd m = vecFold gcd (maxElemVec m) m


-- matHom :: (Integral a) =>  Integer -> SqMatrix a -> SqMatrix a
-- matHom :: (Integral a) => Integer -> SqMatrix a -> SqMatrix a

matHom p = matMapUnary (modHom p)

-- vecHom :: (Integral a) => Integer -> Vector a -> Vector a
-- vecHom :: (Integral a) => Integer -> Vector a -> Vector a

vecHom p (VectorC _ v) = vector (mat_map (modHom p) v) 

{-
matBounds :: (Integral a) => SqMatrix a -> MatBounds

matBounds (SqMatrixC mat) = bounds mat
-}

matFromInteger :: Integer -> SqMatrix Integer

matFromInteger n = SqMatrixC ((1,1),(1,1)) [[n]]

-- ----------------------------------------------------------------------------
-- I/O Operations
-- ----------------------------------------------------------------------------

-- showsMatrix :: (Ix a, Ix b, Text c) => Array (a,b) c -> ShowS

showsMatrix :: (Integral a) => SqMatrix a -> ShowS

showsMatrix (SqMatrixC _ mat) = ( (++) ("Matrix: \n" ++
                                  (foldl (++) "" [ show line ++ "\n" 
                                                 | line <- mat ] ) ) )


showsVector :: (Integral a) => Vector a -> ShowS

showsVector (VectorC _ vec) = 
	( (++) ("Vector: " ++ show vec) ) 

-- ----------------------------------------------------------------------------
-- Instance definitions for the ADT of Square Matrices and Vectors
-- ----------------------------------------------------------------------------

{-
instance (Eq a) => Eq [a] where
 l == l' = foldl (&&) True (listCompwiseComp (==) l l')
-}

instance (Integral a) => Eq (SqMatrix a) where
 (==) = matEqual

instance (Integral a) => Text (SqMatrix a) where
 readsPrec p  = error "readsPrec of Matrix: Not yet implemented!\n"
 showsPrec p  = showsMatrix

instance (Integral a) => Num (SqMatrix a) where                
 (+) = matSum
 (-) = matDif
 (*) = matMult
 negate = scalarMult (-1)
 abs = matAbs
 signum = matSignum
 fromInteger = error "fromInteger of Matrix: Not yet implemented\n"
               {- matFromInteger -}


instance (Integral a) => Eq (Vector a) where
 (==) = vecEqual

instance (Integral a) => Text (Vector a) where
 readsPrec p  = error "readsPrec of Vector: Not yet implemented!\n"
 showsPrec p  = showsVector


{-
instance  (Integral a) => Hom (Vector a) where
 hom = vecHom
 -- hom p (VectorC v) = vector (map (modHom p) (elems v)) 

instance  (Integral a) => Hom (SqMatrix a) where
 hom = matHom
 --hom p = matMapUnary (modHom p)
-}
