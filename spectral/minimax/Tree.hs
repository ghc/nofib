module Tree where

data Tree a = Branch a [Tree a] deriving Text

repTree :: (a->[a]) -> (a->[a])-> a -> (Tree a)
repTree f g a = Branch a (map (repTree g f) (f a))


mapTree :: (a -> b) -> (Tree a) -> (Tree b)
mapTree f (Branch a l) = Branch (f a) (map (mapTree f) l)

prune :: Int -> (Tree a) -> (Tree a)
prune 0 (Branch a l) = Branch a []
prune (n+1) (Branch a l) = Branch a (map (prune n) l)

