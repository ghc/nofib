{-
 - Arrays.hs
 -
 - An implementation of arrays using a tree
 -
 - Version 0.1 10/09/92 PS
 - Version 0.2 01/10/92 PS Added force function to //^ to reduce space
 - consumption
 -
 -}

module Arrays (Maybe(..), TArray,
               (!^), (//^),
               mkTArray, lookup, isUndefined)
where

infixl 9 !^
infixl 9 //^

--data Maybe a = Nothing | Just a deriving Show{-was:Text-}
data TArray a = Tarray Int Int Int (BinTree a) deriving Show{-was:Text-}
data BinTree a = Leaf a | Branch (BinTree a) (BinTree a) | Empty deriving Show{-was:Text-}
data Direction = Left | Right deriving Show{-was:Text-}

mkTArray ::  Int -> Int -> TArray a
mkTArray lo hi = Tarray lo hi (maxBits 1) Empty
                 where maxBits n = if (hi - lo + 1) > 2^n then
                                        maxBits (n+1)
                                   else
                                      n

(!^) :: TArray a -> Int -> Maybe a
(!^) (Tarray lo hi bits t) index
        = if (index >= lo) && (index <= hi) then
               doLookup (directions bits (index-lo)) t
          else error "Array index out of bounds\n"
          where
          doLookup _ Empty    = Nothing
          doLookup _ (Leaf n) = Just n
          doLookup (Left:ds) (Branch l _)  = doLookup ds l
          doLookup (Right:ds) (Branch _ r) = doLookup ds r

(//^) :: TArray a -> [Assoc Int a] -> TArray a
(//^) t [] = t
(//^) (Tarray lo hi hiIndex t) ((i := v) : as)
        = if (i >= lo) && (i <= hi) then
                (//^) t' as
          else error "Array: index out of bounds\n"
          where
          t' = if (forced dirs newt) then newt else newt
          newt = Tarray lo hi hiIndex (add dirs v t)
          dirs = directions hiIndex (i - lo)
          add [] v _                    = Leaf v
          add (Left:ds) v Empty         = Branch (add ds v Empty) Empty
          add (Left:ds) v (Branch l r)  = Branch (add ds v l) r
          add (Right:ds) v Empty        = Branch  Empty (add ds v Empty)
          add (Right:ds) v (Branch l r) = Branch l (add ds v r)

forced ds (Tarray _ _ _ t) = bforced ds t

bforced [] _ = True
bforced (Left:ds) (Branch l _) = bforced ds l
bforced (Right:ds) (Branch _ r) = bforced ds r
bforced _ _ = True

directions :: Int -> Int -> [Direction]
directions bits index 
      = take (bits - length dirString) lefts ++ reverse dirString
        where dirString = convert index

convert 0 = []
convert x = case (x `rem` 2) of
              1 -> Right : convert (x `div` 2)
              0 -> Left  : convert (x `div` 2)

lefts = repeat Left

lookup :: Int -> TArray a -> a
lookup i a = case (a!^i) of
               Nothing -> error "No value to lookup\n"
               Just n -> n

isUndefined :: Int -> TArray a -> Bool
isUndefined i a = case (a!^i) of
                    Nothing -> True
                    Just _  -> False
