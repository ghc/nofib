{-
   **************************************************************
   * Filename      : Utils.hs                                   *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 22 July, 2001                              *
   * Lines         : 66                                         *
   **************************************************************
-}

module FST.Utils (
           cross, -- cross product of two lists.
           insert,
           merge,
           remove,
           tagging
           ) where

{- **********************************************************
   * cross: cartesian product of two lists.                 *
   **********************************************************
-}


cross :: [a] -> [b] -> [(a,b)]
cross as bs = [(a,b) | a <- as, b <- bs]

{- **********************************************************
   * insert, merge, remove: aux. functions for transition   *
   * tables.                                                *
   **********************************************************
-}

{-# SPECIALIZE insert :: (Int,[(String,Int)]) -> [(Int,[(String,Int)])] -> [(Int,[(String,Int)])] #-}

insert :: Eq b => (b,[(a,b)]) -> [(b,[(a,b)])] -> [(b,[(a,b)])]
insert (s,t1) [] = [(s,t1)]
insert (s,t1) ((s1,t2):xs)
 | s == s1    = (s1, t1++t2):xs
 | otherwise  = (s1,t2):insert (s,t1) xs

{-# SPECIALIZE merge :: [(Int,[(String,Int)])] -> [(Int,[(String,Int)])] -> [(Int,[(String,Int)])] #-}

merge :: Eq b => [(b,[(a,b)])] -> [(b,[(a,b)])] -> [(b,[(a,b)])]
merge [] table2 = table2
merge (a:table1) table2 = merge table1 (insert a table2)

{-# SPECIALIZE remove :: Int -> [(Int,[(String,Int)])] -> [(Int,[(String,Int)])] #-}

remove :: Eq b => b -> [(b,[(a,b)])] -> [(b,[(a,b)])]
remove _ [] = []
remove s ((s1,tl):xs)
  | s == s1 = xs
  | otherwise = (s1,tl):remove s xs

{- **********************************************************
   * tagging: Tag a list of polymorphic type with integers. *
   **********************************************************
-}

tagging :: [a] -> Int -> (Int,[(a,Int)])
tagging xs s            = tag xs s []
 where tag    []  s1 ys = ((s1-1),ys)
       tag (a:zs) s1 ys = tag zs (s1+1) ((a,s1):ys)
