module Util where

import Data.List(sortBy)
import System.IO (stderr,hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)

-- quick and dirty logging of debug info
_log :: String -> ()
_log    = unsafePerformIO . hPutStrLn stderr

-- strict foldl
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ a []     = a
foldl' f a (x:xs) = (foldl' f $! f a x) xs

sortOn :: Ord b => (a->b) -> [a] -> [a]
sortOn f = sortBy (\x y -> compare (f x) (f y))

-- break a list into all segments at a predicate
breaks' :: (a->Bool) -> [a] -> [[a]]
breaks' _ [] = []
breaks' p xs = let (first,rest) = break' p xs in
   first : breaks' p rest

-- break a list at first occurence of predicate
break' :: (a->Bool) -> [a] -> ([a],[a])
break' p (x:xs) = if p x then let (a,b) = break p xs in (x:a,b)
		  else break p (x:xs)
break' _ [] = ([],[])

