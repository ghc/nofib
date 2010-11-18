module Data.Map.TernaryMap.Internal where

-- | TernaryMap k v is ternary tree. It is commonly used for storing word lists
-- like dictionaries.
data TernaryMap k v = 
                    -- | Nodes contain key elements only
                      Node !k !(TernaryMap k v) !(TernaryMap k v) !(TernaryMap k v)
                    -- | Null nodes contain the value pointed to by the key list
                    | Null  v !(TernaryMap k v)
                    -- | An empty tree
                    | End
               deriving (Show, Eq)