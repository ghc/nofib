
module Data.Map.StringMap where
import Data.Binary
import Control.Monad


-- | Elem2 a b is used to hold elements of a list after insertion, and
-- indicate that we've reached the end of the list.
data CharEl a b = C !Char | Val b
             deriving (Show)
-- | StringMap a is ternary tree. It is commonly used for storing word lists
-- like dictionaries for spell checking etc.
data StringMap a = SNode {-# UNPACK #-}!Char !(StringMap a) !(StringMap a) !(StringMap a)
                 | SElemNode a !(StringMap a) !(StringMap a) !(StringMap a)
                 | SEnd
               deriving (Show, Eq)


-- | Inserts a new list of elements into a tree.
insert :: String -> a -> StringMap a -> StringMap a
-- General case
insert xss@(x:xs) val (SNode ele l e h) =
    case compare x ele of
        LT -> SNode ele (insert xss val l) e h
        EQ -> SNode ele l (insert xs val e) h
        GT -> SNode ele l e (insert xss val h)
        
-- Insert new elements quickly
insert xss@(x:xs) val SEnd =
    insert' xss val
    
-- SEnd of word in non empty tree
insert [] val t@(SNode ele l e h) = 
    case compare '\0' ele of
        EQ -> t
        LT  -> SNode ele (insert [] val l) e h
        
-- SEnd of word in empty tree
insert [] val SEnd =
    SElemNode val SEnd SEnd SEnd

-- | Quickly build an initial tree.
insert' :: String -> a -> StringMap a
insert' (x:xs) a = SNode x SEnd (insert' xs a) SEnd
insert' []     a = SElemNode a SEnd SEnd SEnd

-- | Returns true if the string is in the StringMap
isElem :: String -> StringMap a -> Bool
isElem          _ SEnd              = False
isElem         [] (SNode ele l e h) = ele == '\0' || isElem [] l
isElem xss@(x:xs) (SNode ele l e h) = 
    case compare x ele of
        LT -> isElem xss l
        EQ -> isElem  xs e
        GT -> isElem xss h

-- | Returns the number of non-Null Elems
treeSize :: StringMap a -> Int
treeSize SEnd = 0
treeSize (SElemNode _ l e h) = treeSize l + treeSize e + treeSize h
treeSize (SNode _ l e h) = 1 + treeSize l + treeSize e + treeSize h

-- | Counts how many entries there are in the tree.
numEntries :: StringMap a -> Int
numEntries SEnd = 0
numEntries (SElemNode _ l e h) = 1 + numEntries l + numEntries e + numEntries h
numEntries (SNode _ l e h) = numEntries l + numEntries e + numEntries h

-- | Creates a new tree from a list of 'strings'
fromList :: [(String,a)] -> StringMap a
fromList = foldl (\t (k,v) -> insert k v t) SEnd


instance Binary a => Binary (StringMap a) where
    put SEnd = put (0 :: Word8)
    -- Quite common, so speecialised
    put (SNode ch SEnd SEnd SEnd) = do
        putWord8 1
        put ch
    -- Also common, basically what insert' produces.
    put (SNode ch SEnd e SEnd) = do
        putWord8 2
        put ch
        put e
    -- General case
    put (SNode ch l e h) = do
        putWord8 3
        put ch
        put l
        put e
        put h
    get = do
        tag <- getWord8
        case tag of
            0 -> return SEnd
            1 -> do
                ch <- get
                return (SNode ch SEnd SEnd SEnd)
            2 -> do
                ch <- get
                e <- get
                return (SNode ch SEnd e SEnd)
            3 -> liftM4 SNode get get get get




