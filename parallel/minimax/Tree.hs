module Tree where

import ParForce

data Tree a = Branch a [Tree a] deriving Text

repTree :: (a->[a]) -> (a->[a])-> a -> (Tree a)
repTree f g a = Branch a (par_map 12 (repTree g f) (f a))

#ifndef SEQ

mapTree :: (a -> b) -> (Tree a) -> (Tree b)
mapTree f (Branch a l) = parGlobal 14 14 1 0
			   fa
			   (Branch fa (par_map 13 (mapTree f) l))
			 where fa = f a

#else  {- SEQ -}

mapTree :: (a -> b) -> (Tree a) -> (Tree b)
mapTree f (Branch a l) = Branch (f a) (map (mapTree f) l)

#endif

prune :: Int -> (Tree a) -> (Tree a)
prune 0 (Branch a l) = Branch a []
prune (n+1) (Branch a l) = Branch a (map (prune n) l)

