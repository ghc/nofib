
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.Minimize
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005
-- License     :  LGPL
--
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Minimization of the States of a Deterministica Finite Automata
--
-- Code Included in the Lecture Notes on
--      Language Processing (with a functional flavour).
--
-----------------------------------------------------------------------------

module Language.HaLex.Minimize (
                -- * Minimization
                  minimizeDfa
                , stdMinimizeDfa
                , minimizeExp
                , minimizeNdfa
                -- * Reversing Automata
                , reverseDfa
                , reverseDfaAsDfa
                , reverseNdfa
                ) where

import Data.List
import Language.HaLex.Util
import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.FaOperations


-----------------------------------------------------------------------------
-- * Minimization


-- | Minimize the number of states of a given 'Dfa'.
--   This function uses Brzozowski's algorithm

minimizeDfa :: (Eq sy, Ord st)
            => Dfa st sy              -- ^ Original 'Dfa'
            -> Dfa [[st]] sy          -- ^ Equivalent Minimized 'Dfa'

minimizeDfa = ndfa2dfa . reverseDfa . ndfa2dfa . reverseDfa


-- | Minimize the number of states of a given 'Ndfa'.
--   This function uses Brzozowski's algorithm

minimizeNdfa :: (Eq sy, Ord st)
             => Ndfa st sy            -- ^ Original 'Ndfa'
             -> Dfa [[st]] sy         -- ^ Equivalent Minimized 'Dfa'

minimizeNdfa =  ndfa2dfa . reverseDfa . ndfa2dfa . reverseNdfa


-- | Minimize the number of states of a given 'Dfa'.
--
-- This minimization algorithm is described in
--   \"An Introduction to Formal Languages and Automata\", Peter Linz, 3rd Ed.
--   Jones and Bartlett Publishers
--

stdMinimizeDfa :: (Ord st, Ord sy)
               => Dfa st sy                  -- ^ Original 'Dfa'
               -> Dfa [st] sy                -- ^ Equivalent Minimized 'Dfa'
stdMinimizeDfa dfa = ttDfa2Dfa  (vs,und,bl ss ,z',list)
  where a = removeinaccessible dfa
        Dfa vs qs ss zs ds = a
        dist = distinguishable a
        und  = undistinguishable a dist
        tt = [ (q,v,z) | q <- qs , v <- vs , z <- [ds q v] ]
        list = nub [ (bl x , y , bl z ) | (x,y,z) <- tt , bl x /= [] , bl z/= [] ]
        z' = nub (filter (/=[]) (map bl zs))

        bl x = concat ([] : (filter (x `elem`) und))


undistinguishable :: Eq st => Dfa st sy -> [(st,st)] -> [[st]]
undistinguishable (Dfa _ qs _ _ _) dist =
        eraseintersect [ i : [j | j <- qs , ne(i,j) , ne (j,i), j/= i ] | i <- qs ]
  where ne p = not (p `elem` dist)


eraseintersect :: Eq a => [[a]] -> [[a]]
eraseintersect []     = []
eraseintersect (x:xs) | (concat . map (x `intersect`)) xs == []  = x : eraseintersect xs
                      | otherwise                                = eraseintersect xs


distinguishable :: Eq st => Dfa st sy -> [(st,st)]
distinguishable (Dfa vs qs _ zs delta) = limit (\x -> nub (x ++ nthdist qs x)) (fstdist qs)
  where fstdist []     = []
        fstdist (x:xs) = [(x,a) | a <- xs , (x `elem` zs) /= (a `elem` zs) ] ++ fstdist xs

        nthdist []     _    = []
        nthdist (a:as) dist = [ (a,b) | b <- qs , x <- vs , move2Disting (a,b) x dist ]
                              ++ nthdist as dist

        move2Disting (a,b) x dist = (delta a x , delta b x) `elem` dist


removeinaccessible dfa@(Dfa v s i f d) = Dfa v states i (f `intersect` states) d
  where states = nub (i:(flowdown [i] (transitionTableDfa dfa)))
        flowdown zz ss = limit (\ x -> nub(x ++ nextlevel ss x)) (nextlevel ss zz)
        nextlevel ss zz = (nub . concat) [ next z ss | z <- zz ]
        next z = concat . map (\(a,_,c) -> if a == z then [c] else [])

removeinaccessible' a = ndfa2dfa . dfa2ndfa $ a




-- | Minimize the number of states of a given 'Dfa'.
--
--   (a third algorithm)

minimizeExp :: Ord st
            => Dfa st sy                      -- ^ Original 'Dfa'
            -> Dfa [st] sy                    -- ^ Equivalent Minimized 'Dfa'

minimizeExp (Dfa t lst si lsf d) = Dfa t l (head (filter (\x->elem si x) l))
                                        (filter (\x->intersect x lsf /= []) l) ndelta
                                where (a,b)=partition f lst
			              f x = elem x lsf
				      l = (minaux lst d t) [a,b]
				      ndelta st s | elem st l = rfind (d (head st) s) l
					          | otherwise = []

rfind :: Eq a => a -> [[a]] -> [a]
rfind _ []=[]
rfind x (h:t)| elem x h  = h
             | otherwise = rfind x t

minaux :: (Ord a) => [a] -> (a -> b -> a) -> [b] -> [[a]] -> [[a]]
minaux lst d simb p | p == p' = p
                    | otherwise = minaux lst d simb p'
		    where p' =concatMap (partes lst d simb p []) p

partes :: Eq a => [a] -> (a -> b -> a) -> [b] -> [[a]] -> [a] -> [a] -> [[a]]
partes _ _ _ _ _ []  =[]
partes _ _ _ _ ac [h] | elem h ac = []
                      | otherwise = [[h]]
partes lst d simb p ac (h:hs) |(elem h ac) = partes lst d simb p ac hs
                              |otherwise = ([h]++r):(partes lst d simb p  (ac++r) hs)
			      where r = raux hs
			            raux []=[]
		                    raux (x:xs) | (comparaDelta lst d simb p h x) = x:(raux xs)
			                        | otherwise = raux xs


mesmoGrupo :: Eq a => [a] -> [[a]] -> a -> a -> Bool
mesmoGrupo lst [] s t = (elem s lst) == (elem t lst)
mesmoGrupo lst (h:t) x y | ((elem x h) && (elem y h)) = True
                         | otherwise = mesmoGrupo lst t x y

comparaDelta :: Eq a => [a] -> (a -> b -> a) -> [b] -> [[a]] -> a -> a -> Bool
comparaDelta lst d simb p s t= and (map (comparaDeltaSimb lst d p s t) simb)



comparaDeltaSimb :: Eq a => [a] -> (a -> b -> a) -> [[a]] -> a -> a -> b -> Bool
comparaDeltaSimb lst d p s t v = mesmoGrupo lst p s' t'
                                 where s' = d s v
				       t' = d t v


-----------------------------------------------------------------------------
-- * Reversing Automata



-- | Reverse a 'Dfa'

reverseDfa :: Eq st
           => Dfa st sy               -- ^ Original 'Dfa'
           -> Ndfa st sy              -- ^ Resulting 'Ndfa'

reverseDfa (Dfa v qs s z delta) = Ndfa v qs z [s] delta'
  where delta' st (Just sy) = [ q | q <- qs
                                  , delta q sy == st ]
        delta' st Nothing   = []


-- | Reverse a 'Ndfa'

reverseNdfa :: Eq st
            => Ndfa st sy             -- ^ Original 'Ndfa'
            -> Ndfa st sy             -- ^ Resulting 'Ndfa'
reverseNdfa (Ndfa v qs s z delta) = Ndfa v qs z s delta'
  where delta' st sy = [ q | q <- qs
                           , st `elem` delta q sy  ]

-- | Reverse a 'Dfa' into a 'Dfa'. It uses a 'Ndfa' as an intermediate representation.

reverseDfaAsDfa :: (Ord st , Eq sy)
                => Dfa st sy          -- ^ Orginal 'Dfa'
                -> Dfa [st] sy        -- ^ Resulting 'Dfa'
reverseDfaAsDfa = ndfa2dfa . reverseDfa



