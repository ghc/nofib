-- this is from ghc/syslib-ghc

-- this is patched !!!

-- renamed union -> unionSet, intersect -> intersectSet
-- because it clashes with List.hs

-- included filterSet function
-- included mergeFM function

-- added some specialisations

module Set (
	-- not a synonym so we can make it abstract
	Set,

	mkSet, setToList, emptySet, singletonSet, unitSet,
	unionSet, unionManySets, minusSet,
	elementOf, mapSet,
	intersectSet, isEmptySet,
	intersectManySets,
	cardinality,
	
	filterSet,

	mergeFM

    ) where


import FiniteMap
import Maybes


infixl 5 `unionSet`
infixl 6 `intersectSet`


-- import TA -- for specializations

-- just to see if and how this works

{- # SPECIALIZE mkSet :: [Int] -> Set Int #-}
{- # SPECIALIZE mkSet :: [(Int, Int)] -> Set (Int, Int) #-}
{- # SPECIALIZE mkSet :: [STerm Int] -> Set (STerm Int) #-}

{- # SPECIALIZE setToList :: Set Int -> [Int] #-}
{- # SPECIALIZE setToList :: Set (Int, Int) -> [(Int, Int)] #-}
{- # SPECIALIZE setToList :: Set (STerm Int) -> [(STerm Int)] #-}

--------------------------------------------------------------------

-- This can't be a type synonym if you want to use constructor classes.
newtype Set a = MkSet (FiniteMap a ())

emptySet :: Set a
emptySet = MkSet emptyFM

unitSet :: a -> Set a
unitSet x = MkSet (unitFM x ())
singletonSet = unitSet -- old;deprecated?

setToList :: Set a -> [a]
setToList (MkSet set) = keysFM set

mkSet :: Ord a => [a]  -> Set a
mkSet xs = MkSet (listToFM [ (x, ()) | x <- xs])

unionSet :: Ord a => Set a -> Set a -> Set a
unionSet (MkSet set1) (MkSet set2) = MkSet (plusFM set1 set2)

unionManySets :: Ord a => [Set a] -> Set a
unionManySets ss = foldr unionSet emptySet ss

minusSet  :: Ord a => Set a -> Set a -> Set a
minusSet (MkSet set1) (MkSet set2) = MkSet (minusFM set1 set2)

intersectSet :: Ord a => Set a -> Set a -> Set a
intersectSet (MkSet set1) (MkSet set2) = MkSet (intersectFM set1 set2)

intersectManySets :: Ord a => [Set a] -> Set a
intersectManySets [] = emptySet -- STRANGE
intersectManySets ss = foldr1 intersectSet ss

elementOf :: Ord a => a -> Set a -> Bool
elementOf x (MkSet set) = exists (lookupFM set x)

isEmptySet :: Set a -> Bool
isEmptySet (MkSet set) = sizeFM set == 0

mapSet :: Ord a => (b -> a) -> Set b -> Set a
mapSet f (MkSet set) = MkSet (listToFM [ (f key, ()) | key <- keysFM set ])

cardinality :: Set a -> Int
cardinality (MkSet set) = sizeFM set

filterSet :: Ord a => (a -> Bool) -> Set a -> Set a
filterSet p (MkSet set) = MkSet (filterFM (\ x _ -> p x) set)


mergeFM :: (Ord a, Ord b) => 
	FiniteMap a (Set b) -> FiniteMap a (Set b) -> FiniteMap a (Set b)
mergeFM l r = plusFM_C unionSet l r


-- fair enough...
instance (Eq a) => Eq (Set a) where
  (MkSet set_1) == (MkSet set_2) = set_1 == set_2
  (MkSet set_1) /= (MkSet set_2) = set_1 /= set_2

-- but not so clear what the right thing to do is:
{- NO:
instance (Ord a) => Ord (Set a) where
  (MkSet set_1) <= (MkSet set_2) = set_1 <= set_2
-}

instance Ord a => Ord (Set a) where
	s <= t = setToList s <= setToList t


instance Show a => Show (Set a) where 
    showsPrec p s = 
	showsPrec p (setToList s)

