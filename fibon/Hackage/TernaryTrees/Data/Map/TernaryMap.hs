module Data.Map.TernaryMap (
                TernaryMap,
                insert,
                singleton,
                member,
                size,
                fromList,
                lookup,
                (!),
                findWithDefault,
                insertWith,
                insertWithKey,
                keys,
                assocs,
                elems,
                null
                ) where
import Data.Map.TernaryMap.Internal
import Data.Bits
import Data.Binary
import Control.Monad
import Control.Arrow (first)
import Prelude hiding (null,lookup)



-- | Quickly build a tree without an initial tree. This should be used
-- to create an initial tree, using insert there after.
singleton :: Ord k => [k] -> v -> TernaryMap k v
singleton (x:xs) v = Node x End (singleton xs v) End
singleton []     v = Null v End

-- | Inserts an entrie into a tree. Values with the same key will be replaced
-- with the newer value.
insert :: Ord k => [k] -> v -> TernaryMap k v -> TernaryMap k v
insert xss@(_:_)  v End              = singleton xss v
insert xss@(_:_)  v (Null v' rest)   = Null v' $ insert xss v rest
insert []         v End              = Null v End
insert []         v (Node ele l e h) = Node ele (insert [] v l) e h
insert []         v (Null _ rest)    = Null v rest
insert xss@(x:xs) v (Node ele l e h) =
    case compare x ele of
        LT -> Node ele (insert xss v l) e h
        EQ -> Node ele l (insert xs v e) h
        GT -> Node ele l e (insert xss v h)


-- | Inserts a new value into the tree with a given function that combines the new value
-- and the old value together to for a new entry.
-- 
-- > insertWith f key newval (fromList [(notkey,val1),(key,oldval)]) == fromList [(notkey,val1),(key,f newval oldval)]
insertWith :: Ord k => (v -> v -> v) -> [k] -> v -> TernaryMap k v -> TernaryMap k v
insertWith _ xss@(_:_)  v End              = singleton xss v
insertWith f xss@(_:_)  v (Null v' rest)   = Null (f v v') $ insertWith f xss v rest
insertWith _ []         v End              = Null v End
insertWith f []         v (Node ele l e h) = Node ele (insertWith f [] v l) e h
insertWith _ []         v (Null _ rest)    = Null v rest
insertWith f xss@(x:xs) v (Node ele l e h) =
    case compare x ele of
        LT -> Node ele (insertWith f xss v l) e h
        EQ -> Node ele l (insertWith f xs v e) h
        GT -> Node ele l e (insertWith f xss v h)


-- | Inserts a new value into the tree with a given function that combines the new value
-- and the old value together to for a new entry.
-- 
-- > insertWithKey f key newval (fromList [(notkey,val1),(key,oldval)]) == fromList [(notkey,val1),(key,f key newval oldval)]
insertWithKey :: Ord k => ([k] -> v -> v -> v) -> [k] -> v -> TernaryMap k v -> TernaryMap k v
insertWithKey f ks v m = insertWith (f ks) ks v m
-- | Returns true if the `[k]` is a key in the TernaryMap.
member :: Ord k => [k] -> TernaryMap k v -> Bool
member         _   End             = False
member         [] (Null _ _)       = True
member         [] (Node _ l _ _)   = member [] l
member xss@(_:_)  (Null _ rest)    = member xss rest
member xss@(x:xs) (Node ele l e h) = 
    case compare x ele of
        LT -> member xss l
        EQ -> member  xs e
        GT -> member xss h


lookup :: Ord k => [k] -> TernaryMap k v -> Maybe v
lookup _ End                       = Nothing
lookup [] (Null v _)               = Just v
lookup [] (Node _ l _ _)           = lookup [] l
lookup xs (Null _ rest)            = lookup xs rest
lookup xss@(x:xs) (Node ele l e h) =
    case compare x ele of
        LT -> lookup xss l
        EQ -> lookup  xs e
        GT -> lookup xss h

(!) :: Ord k => TernaryMap k v -> [k] -> Maybe v
(!) = flip lookup

findWithDefault :: Ord k => v -> [k] -> TernaryMap k v -> v
findWithDefault k _ End                       = k
findWithDefault _ [] (Null v _)               = v
findWithDefault k [] (Node _ l _ _)           = findWithDefault k [] l
findWithDefault k xs (Null _ rest)            = findWithDefault k xs rest
findWithDefault k xss@(x:xs) (Node ele l e h) =
    case compare x ele of
        LT -> findWithDefault k xss l
        EQ -> findWithDefault k  xs e
        GT -> findWithDefault k xss h

-- | Returns the number of non-Val Elems. not exported
treeSize :: TernaryMap k v -> Int
treeSize End = 0
treeSize (Node _ l e h) = 1 + treeSize l + treeSize e + treeSize h
treeSize (Null _ rest) = treeSize rest

-- | Counts how many entries there are in the tree.
size :: TernaryMap k v -> Int
size End            = 0
size (Node _ l e h) = size l + size e + size h
size (Null _ rest)  = 1 + size rest

-- | Creates a new tree from a list of 'strings'
fromList :: Ord k => [([k],v)] -> TernaryMap k v
fromList = foldl (\tree (as,v) -> insert as v tree) empty

-- | An empty map.
empty :: TernaryMap k v
empty = End

-- | Makes a list of all the values in the map.
elems :: TernaryMap k v -> [v]
elems  End           = []
elems (Node _ l e h) = elems l ++ (elems e ++ elems h)
elems (Null v rest)  = v : elems rest

-- | Returns a (sorted) list of all keys in the map.
keys :: TernaryMap k v -> [[k]]
keys End              = []
keys (Null _ rest)    = []:keys rest
keys (Node ele l e g) = keys l ++ map (ele:) (keys e) ++ keys g


-- | Returns a (sorted) list of all keys in the map.
assocs :: TernaryMap k v -> [([k],v)]
assocs  End             = []
assocs (Null v rest)    = ([],v):assocs rest
assocs (Node ele l e g) = assocs l ++ map (first (ele:)) (assocs e) ++ assocs g

-- | Returns true if the map is empty.
null :: TernaryMap k v -> Bool
null End = True
null _   = False

-- keySet :: TernaryMap k v -> S.TernarySet a
-- keySet End = S.End
-- keySet (Node (C x) l e h) = S.Node (S.C x) (keySet l) (keySet e) (keySet h)
-- keySet (Node (Val _) l e h) = S.Node (S.Null) (keySet l) (keySet e) (keySet h)


instance Functor (TernaryMap k) where
    fmap _ End = End
    fmap f (Null v rest)    = Null (f v) (fmap f rest)
    fmap f (Node ele l e h) = Node ele (fmap f l) (fmap f e) (fmap f h)

-- | A rather long Binary instance, that uses binary numbers to indicate
-- where Ends are efficiently.
instance (Binary k, Binary v) => Binary (TernaryMap k v) where
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
    put (Null v End) = putWord8 8 >> put v
    put (Null v rest) = do
        putWord8 9
        put v
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
            8 -> liftM (flip Null End) get
            9 -> liftM2 Null get get
            10 -> return End
            _ -> error ("Invalid data in binary stream. tag: " ++ show tag) 
            