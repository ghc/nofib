module Data.Set.TernarySet (
                TernarySet,
                insert,
                singleton,
                member,
                size,
                fromList,
                null,
                elems,
                
                ) where
import Prelude hiding (null)
import Data.Bits
import Data.Binary
import Control.Monad
import Data.Set.TernarySet.Internal


-- | Quickly build a tree without an initial tree. This should be used
-- to create an initial tree, using insert there after.
singleton :: Ord a => [a] -> TernarySet a
singleton (x:xs) = Node x End (singleton xs) End
singleton []     = Null End

-- | Inserts an entries into a tree.
insert :: Ord a => [a] -> TernarySet a -> TernarySet a
insert xss@(_:_)  End              = singleton xss
insert xss@(_:_)  (Null rest)      = Null $ insert xss rest
insert []         End              = Null End
insert []         (Node ele l e h) = Node ele (insert [] l) e h
insert []         (Null rest)      = Null rest
insert xss@(x:xs) (Node ele l e h) =
    case compare x ele of
        LT -> Node ele (insert xss l) e h
        EQ -> Node ele l (insert xs e) h
        GT -> Node ele l e (insert xss h)


-- | Returns true if the `[a]` is in the TernarySet
member :: Ord a => [a] -> TernarySet a -> Bool
member         _   End             = False
member         [] (Null _)         = True
member         [] (Node _ l _ _)   = member [] l
member xss@(_:_)  (Null rest)      = member xss rest
member xss@(x:xs) (Node ele l e h) = 
    case compare x ele of
        LT -> member xss l
        EQ -> member  xs e
        GT -> member xss h


-- | Counts how many entries there are in the tree.
size :: TernarySet a -> Int
size End = 0
size (Null rest) = 1 + size rest
size (Node _ l e h) = size l + size e + size h

-- | Creates a new tree from a list of 'strings'
fromList :: Ord a => [[a]] -> TernarySet a
fromList = foldl (flip insert) empty

-- | Returns a (sorted) list of all elements inserted into the set.
elems :: TernarySet a -> [[a]]
elems End = []
elems (Null rest) = []:elems rest
elems (Node ele l e g) = elems l ++ (map (ele:) (elems e) ++ elems g)

-- | An empty set.
empty :: TernarySet a 
empty = End

-- | Returns true if the set is empty.
null :: TernarySet a -> Bool
null End = True
null _    = False

-- instance Binary a => Binary (Elem a) where
--     put Null = putWord8 0
--     put (C x) = putWord8 1 >> put x
--     
--     get = do
--         n <- getWord8
--         case n of
--             0 -> return Null
--             1 -> liftM C get

-- | This binary uses the fact that the number of Ends can be represented
-- in binary numbers to save a lot of space.
instance Binary a => Binary (TernarySet a) where
    put (Node ch End End End) = do
        putWord8 0
        put ch
    put (Node ch End End h) = do
        putWord8 1
        put ch
        put h
    put (Node ch End e End) = do
        putWord8 2
        put ch
        put e
    put (Node ch End e h) = do
        putWord8 3
        put ch
        put e
        put h
    put (Node ch l End End) = do
        putWord8 4
        put ch
        put l
    put (Node ch l End h) = do
        putWord8 5
        put ch
        put l
        put h
    put (Node ch l e End) = do
        putWord8 6
        put ch
        put l
        put e
    -- General case
    put (Node ch l e h) = do
        putWord8 7
        put ch
        put l
        put e
        put h
    put (Null End) = putWord8 8
    put (Null rest) = do
        putWord8 9
        put rest
    put End = putWord8 10
    
    get = do
        tag <- getWord8
        case tag of
            _ | tag < 8 ->
                do
                    ch <- get
                    l <- if tag `testBit` 2 then get else return End
                    e <- if tag `testBit` 1 then get else return End
                    h <- if tag `testBit` 0 then get else return End
                    return (Node ch l e h)
            8 -> return (Null End)
            9 -> liftM Null get
            10 -> return End
            _ -> error ("Invalid data in binary stream. tag: " ++ show tag)

