-- Time-stamp: <Wed Mar 21 2001 17:07:44 Stardate: [-30]6363.56 hwloidl>
--
-- ADT of a binary tree (values only in leaves)
-- ---------------------------------------------------------------------------

#if 0

-- Currently unused; should go into another dir!

module Tree(Tree, list2tree, tree2list, (^:), tree_map, tree_fold, force_tree,
	    depth, create_forest, par_tree_map) where

import Parallel

infixl 2 ^:

data (Integral a) => Tree a = Leaf a
			    | Node (Tree a) (Tree a)
			    deriving (Eq, Text)

--tree_map :: (Integral a, b) => (a -> b) -> Tree a -> Tree b
tree_map f (Leaf x) 		= Leaf (f x)
tree_map f (Node left right) 	= Node (tree_map f left) (tree_map f right)

--par_tree_map :: (Integral a, b) => (a -> b) -> Tree a -> Tree b

#if defined(HBCPP) || defined(PAR) || defined(SEQ)
par_tree_map f (Leaf x) 		= Leaf (f x)
par_tree_map f (Node left right) 	= par left'
					   (par right' 
					    (Node left' right'))
					  where left' = par_tree_map f left
						right' = par_tree_map f right
#elif GRAN
-- NB: par can be used instead of parGlobal but with the latter we can
--     give different names to the left and the right branch
par_tree_map f (Leaf x) 		= Leaf (f x)
par_tree_map f (Node left right) 	= parGlobal 31 31 0 0 left'
					   (parGlobal 32 32 0 0 right' 
					    (Node left' right'))
					  where left' = par_tree_map f left
						right' = par_tree_map f right
#else
par_tree_map = tree_map
force_tree = id
# endif 

force_tree t@(Leaf x) = x `seq` t
force_tree t@(Node left right) = (force_tree left) `par` 
	                         (force_tree right) `seq` 
				 t

tree_fold :: (Integral a) => (a -> a -> a) -> a -> Tree a -> a
tree_fold o z (Leaf x) 		= z `o` x
tree_fold o z (Node left right) = tree_fold o z' right
				  where z' = tree_fold o z left

list2tree :: (Integral a) => [a] -> Tree a 
list2tree [] 	= error "list2tree: empty list"
list2tree [x] 	= Leaf x
list2tree l     = Node (list2tree left) (list2tree right)
		  where (left,right) = splitAt ((length l) `div` 2 ) l

tree2list :: (Integral a) => Tree a -> [a]
tree2list (Leaf x) 	= [x]
tree2list (Node left right) = tree2list left ++ tree2list right

(^:) :: Tree a -> Tree a -> Tree a
t1 ^: t2 = Node t1 t2

depth :: Tree a -> Int
depth (Leaf _)		= 0
depth (Node left right) = max (depth left) (depth right) + 1

-- The following functions are useful for the prune.hs test function
create_forest :: (Integral a) => Tree a -> [Tree a] 
create_forest (Leaf x) 		= [ (Leaf y) | y <- [2..x], gcd x y == 1 ]
create_forest (Node left right) = [ (Node left' right') 
				  | left' <- create_forest left,
				    right' <- create_forest right]

#endif
