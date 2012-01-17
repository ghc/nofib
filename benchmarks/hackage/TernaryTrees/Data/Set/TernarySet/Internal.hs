module Data.Set.TernarySet.Internal where

-- | TernarySet a is ternary tree. It is commonly used for storing word lists
-- like dictionaries.
data TernarySet a = 
                  -- | A Node has a less than, equal and greater than branch
                    Node !a !(TernarySet a) !(TernarySet a) !(TernarySet a)
                  -- | Null nodes only have a greater nbranch, by definition
                  | Null !(TernarySet a)
                  -- | An empty tree
                  | End
               deriving (Show, Eq)

-- | Returns the number of non-Null Elems.
treeSize :: TernarySet a -> Int
treeSize End = 0
treeSize (Null rest) = treeSize rest
treeSize (Node _ l e h) = 1 + treeSize l + treeSize e + treeSize h