{-
    Tree e is the type of binary trees with keys of type Key, 
    containing records (entities) of type e.  The tree is
    ordered, but not necessarily balanced.
-}

data Tree e = Node Key (Tree e) (Tree e) | Leaf Key e | Empty deriving Text
type Key = Int


{-
    The Maybe type is used to distinguish success or failure.
-}

data Maybe a = Succ a | Fail


{-
    Interesting entities are 3-tuples of integers.  
    The joins of two such entities are 5-tuples of integers.
-}

type Entity = (Int,Int,Int)
type Join = (Int,Int,Int,Int,Int)

{-
    insertT inserts a (key,entity) pair into a tree.
-}

insertT :: Key -> entity -> Tree entity -> Tree entity
insertT k e (Node k' l r) | k <= k' = Node k' (insertT k e l) r
                          | otherwise = Node k' l (insertT k e r)

insertT k e l@(Leaf k' _) | k < k' = Node k l' l
			  | k > k' = Node k' l l'
			  | otherwise = error ("Key Value " ++ show k ++ " already exists")
			    where l' = Leaf k e

insertT k e Empty = Leaf k e


{-
    "lookupT" looks up the record (entity) whose key is specified,
    in the tree argument.  It returns Succ e if the key value
    exists, or Fail otherwise.
-}

lookupT :: Key -> Tree entity -> Maybe entity
lookupT k (Node k' l r) | k <= k' = lookupT k l
		        | otherwise = lookupT k r

lookupT k (Leaf k' e) | k == k' = Succ e
		      | otherwise = Fail

lookupT k Empty = Fail


{-
    "forceTree" forces a tree to normal form.
-}

forceTree :: Tree Join -> ()
forceTree (Node k l r) | k == k && forceTree l == () && forceTree r == () = ()
forceTree (Leaf k e)   | k == k && e == e = ()
forceTree Empty = ()


{-
    "readTree" reads a tree of entities from a string.  Each entity is
    represented by 3 space-separated positive integers.
    The keys are derived from the entities read in using the function "fk".
-}

readTree :: (Entity->Key) -> String -> Tree Entity -> Tree Entity
readTree fk [] t = t
readTree fk s t = 
    let (f,s') = readInt s; (g,s'') = readInt s'; (h,s''') = readInt s''
	e = (f,g,h)
	k = fk e
    in
	readTree fk s''' (insertT k e t)


readInt :: String -> (Int,String)
readInt s = readInt' 0 s where
	readInt' n s@(c:cs) | isDigit c = readInt' (n*10+ord c-ord '0') cs
	readInt' n s =                    (n,dropWhile isSpace s)


{-
    "join" joins two trees of "Entities" (3-tuples) to produce a
    tree of "Joins" (5-tuples).  The relations are joined on the
    third component of each record.
-}

join :: Tree Entity -> Tree Entity -> Tree Join -> Tree Join
join Empty _ j = j
join _ Empty j = j
join (Leaf k (a,b,c)) t j =  case lookupT c t of
			 Fail -> j
			 Succ (d,e,f) -> insertT c (a,b,c,d,e) j
join (Node k l r) t j = join l t (join r t j)


{-
    The main program reads the two files which are its arguments,
    and joins the relations which those files define.  The result
    of the join is discarded.
-}

main :: Dialogue
main = getArgs abort ( \ (s1 : s2 : _ ) -> run s1 s2)

run :: String -> String -> Dialogue
run f1 f2 =
       readFile f1 abort ( \ c1 ->
       readFile f2 abort ( \ c2 ->
       let a = readTree  (\(x,_,_)->x) c1 Empty
           b = readTree  (\(x,_,_)->x) c2 Empty 
       in
	   print (forceTree (join a b Empty))
--	   print (join a b Empty)
       ))


