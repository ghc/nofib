{-
$Id: ModArithm.hs,v 1.1 1996/01/08 20:07:35 partain Exp $

This is revision: $Revision: 1.1 $

Modular Arithmetic over Integrals with definition of Hom class.
Changelog:
$Log: ModArithm.hs,v $
Revision 1.1  1996/01/08 20:07:35  partain
Initial revision

--# Revision 1.2  1994/11/19  21:50:17  hwloidl
--# *** empty log message ***
--#
--# Revision 1.1  1994/11/19  02:00:05  hwloidl
--# Initial revision
--#
--# Revision 1.1  1994/11/19  02:00:05  hwloidl
--# Initial revision
--#
----------------------------------------------------------------------  -}

module ModArithm(modHom, modSum, modDif, modProd, modInv  {-, Hom(hom) -} )  where

modHom :: (Integral a) => a -> a -> a
modHom m x = x `mod` m

mapMod :: (Integral a) => (a -> a -> a) -> a -> a -> a -> a
mapMod f m x y = modHom m (f x y)

modSum :: (Integral a) => a -> a -> a -> a
modSum = mapMod (+)  

modDif :: (Integral a) => a -> a -> a -> a
modDif = mapMod (-)  

modProd :: (Integral a) => a -> a -> a -> a
modProd = mapMod (*)

modInv _ 0 = 0
modInv m x = let 
               (g,_,inv) = gcdCF m x
             in
               if (g /= 1) 
                 then error ("modInv: Input values are not relative prime:\n " ++ (show m) ++ "\n " ++ (show x) ++"\n") 
                 else modHom m inv

gcdCF x y = gcdCF' x y 1 0 0 1 
            where gcdCF' x 0 x1 x2 _ _   = (x,x1,x2)
                  gcdCF' x y x1 x2 y1 y2 | x<y       = gcdCF' y x y1 y2 x1 x2 
                                         | otherwise = let 
                                                         q  = x `div` y
                                                         z  = x  - q*y
                                                         z1 = x1 - q*y1
                                                         z2 = x2 - q*y2
                                                       in
                                                         gcdCF' y z y1 y2 z1 z2


{- Main for testing gcdCF!

main = let  
         l1 = [7, 12, 62, 54, 55]
         l2 = [3,  9, 30, 48, 15]
         l = map ( \ (x,y) -> gcdCF x y ) (zip l1 l2)
         showTuple (z,x,y) = "(gcd: " ++ show z ++ " Cofactors: " ++ show x ++
                             " , " ++ show y ++ " )\n"
         lshow = foldl (++) "" (map showTuple l)
       in 
         appendChan stdout ("First list: " ++ (showList l1  
                            "\nSecond List: " ++ (showList l2 
                            "\nResult: \n" ++ lshow))) 
         abort done
-}

-- ---------------------------------------------------------------------------
{-

class Hom b where
 hom :: Integer -> b -> b

instance Hom Integer where
 hom = modHom
 --hom m x = x `mod` m

-}