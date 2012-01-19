-- | A module that implements a dictionary/hash table

module Text.Regex.PDeriv.Dictionary where


import qualified Data.IntMap as IM
import Data.Char

class Key a where
    hash :: a -> [Int]


instance Key Int where
    hash i = [i]

instance Key Char where
    hash c = [(ord c)]

instance (Key a, Key b) => Key (a,b) where
    hash (a,b) = hash a ++ hash b

instance (Key a, Key b, Key c) => Key (a,b,c) where
    hash (a,b,c) = hash a ++ hash b ++ hash c


instance Key a => Key [a] where
    hash as = concatMap hash as


-- an immutable dictionary
newtype Dictionary a = Dictionary (Trie a)

primeL :: Int
primeL = 757
primeR :: Int
primeR = 577

empty :: Dictionary a
empty = Dictionary emptyTrie 

-- insert and overwrite
insert :: Key k => k -> a -> Dictionary a -> Dictionary a
insert key val (Dictionary trie) = 
    let key_hash = hash key
    in key_hash `seq` Dictionary (insertTrie True key_hash val trie)

-- insert not overwrite
insertNotOverwrite :: Key k => k -> a -> Dictionary a -> Dictionary a
insertNotOverwrite key val (Dictionary trie) = 
    let key_hash = hash key
    in key_hash `seq` Dictionary (insertTrie False key_hash val trie)



lookup :: Key k => k -> Dictionary a -> Maybe a
lookup key (Dictionary trie) = 
    let key_hash = hash key
    in key_hash `seq` 
       case lookupTrie key_hash trie of
        Just (Trie (x:_) _) -> Just x
	_		    -> Nothing

lookupAll :: Key k => k -> Dictionary a -> [a]
lookupAll key (Dictionary trie) = 
    let key_hash = hash key
    in key_hash `seq` 
       case lookupTrie key_hash trie of
        Just (Trie xs _) -> xs
	_		 -> [] 



fromList :: Key k => [(k,a)] -> Dictionary a 
fromList l = foldl (\d (key,val) -> insert key val d) empty l

fromListNotOverwrite :: Key k => [(k,a)] -> Dictionary a 
fromListNotOverwrite l = foldl (\d (key,val) -> insertNotOverwrite key val d) empty l

update :: Key k => k -> a -> Dictionary a -> Dictionary a
update key val (Dictionary trie) = 
    let key_hash = hash key
        trie'     = key_hash `seq` updateTrie key_hash val trie
    in Dictionary trie'


-- The following are some special functions we implemented for
-- an special instance of the dictionary 'Dictionary (k,a)' 
-- in which we store both the key k together with the actual value a, 
-- i.e. we map (hash k) to list of (k,a) value pairs

-- ^ the dictionary (k,a) version of elem
isIn :: (Key k, Eq k) => k -> Dictionary (k,a) -> Bool
isIn k dict = 
    let all = lookupAll (hash k) dict
    in k `elem` (map fst all)

nub :: (Key k, Eq k) => [k] -> [k]
nub ks = nubSub ks empty

nubSub :: (Key k, Eq k) => [k] -> Dictionary (k,()) -> [k]
nubSub [] d = []
nubSub (x:xs) d 
    | x `isIn` d = nubSub xs d
    | otherwise = let d' = insertNotOverwrite x (x,()) d 
                  in x:(nubSub xs d')




-- An internal trie which we use to implement the dictoinar 

data Trie a = Trie ![a] !(IM.IntMap (Trie a))

emptyTrie = Trie [] (IM.empty)


insertTrie :: Bool -> [Int] -> a -> Trie a -> Trie a
insertTrie overwrite [] i (Trie is maps) 
    | overwrite  =  Trie [i] maps
    | otherwise  =  Trie (i:is) maps
insertTrie overwrite (word:words) i (Trie is maps) = 
    let key = word
    in key `seq` case IM.lookup key maps of 
	 { Just trie -> let trie' = insertTrie overwrite words i trie
			    maps' = trie' `seq` IM.update (\x -> Just trie') key maps
			in maps' `seq` Trie is maps'
	 ; Nothing -> let trie = emptyTrie
			  trie' = insertTrie overwrite words i trie
			  maps' = trie' `seq` IM.insert key trie' maps
		      in maps' `seq` Trie is maps'
	 }




lookupTrie :: [Int] -> Trie a -> Maybe (Trie a) 
lookupTrie [] trie = Just trie
lookupTrie (word:words) (Trie is maps) = 
    let key = word
    in case IM.lookup key maps of
	   Just trie -> lookupTrie words trie
	   Nothing   -> Nothing

-- we only update the first one, not the collided ones
updateTrie :: [Int] -> a -> Trie a -> Trie a
updateTrie [] y (Trie (x:xs) maps) = Trie (y:xs) maps
updateTrie (word:words) v  (Trie is maps) =
    let key = word
    in case IM.lookup key maps of
	   Just trie -> let trie' = updateTrie words v trie
			    maps'  = IM.update (\x -> Just trie') key maps
			in Trie is maps'
	   Nothing   -> Trie is maps



