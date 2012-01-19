-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.Edfa
-- Copyright   :  (c) Jo‹oo Saraiva 2001,2002,2003,2004,2005
-- License     :  LGPL
--
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- HaLeX Utility Functions
--
-- Code Included in the Lecture Notes on
--      Language Processing (with a functional flavour).
--
-----------------------------------------------------------------------------

module Language.HaLex.Util (
             (<->)
           , limit
           , permutations
           ) where

-- | List Function: l1 - l2.
--   Unlike List.(\\), this function removes duplicates as well.
(<->) :: Eq a => [a] -> [a] -> [a]
l1 <-> l2 = [ x | x <- l1 , not (x `elem` l2) ]

-- | Apply a function repeatedly, until a fix point is reached, i.e. until
--   the result of the function is the same as the argument.
limit :: Eq a => (a -> a) -> a -> a
limit f s | s == next = s
          | otherwise = limit f next
            where next = f s


-- | Compute the permutations of a given list. For instance,
--
--    permutations [1,2,3]
--       = [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
--
permutations :: [a] -> [[a]]
permutations []   = [[]]
permutations (x:xs)   = insAllPosLst x x'
  where x' = permutations xs

insAllPosLst :: a -> [[a]] -> [[a]]
insAllPosLst e l = concat $ map (insAllPos e) l

insAllPos :: a -> [a] -> [[a]]
insAllPos e l = [ insAtPos i e l
                | i <- [1..length l + 1]
                ]

insAtPos 1 e l       = e : l
insAtPos i e (x:xs)  = x : insAtPos (i-1) e xs
