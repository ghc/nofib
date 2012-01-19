module Data.Set.StringSet.Internal where
import Data.Ratio

-- | StringSet is ternary tree. It is commonly used for storing word lists
-- like dictionaries for spell checking etc.
data StringSet = 
               -- | Tree node 
               Node {-# UNPACK #-} !Char !StringSet !StringSet !StringSet
               -- | null nodes can only have a greater than branch by definition
               | Null !StringSet 
               -- | a branch that doesn’t contain anything
               | End 
               deriving (Show, Eq)


-- | Returns the number of non-Null Elems
treeSize :: StringSet -> Int
treeSize End = 0
treeSize (Null rest) = treeSize rest
treeSize (Node _ l e h) = 1 + treeSize l + treeSize e + treeSize h

-- -- | A rough comparison of how much space is being used compared to
-- -- the strings as a single string
-- compRatio :: StringSet -> Ratio Int
-- compRatio s = treeSize s % length . concat . elems $ s