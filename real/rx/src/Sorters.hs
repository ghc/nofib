module Sorters 

( msort, msortwith
, oemerge, oemerges
, unimerge, unimerges
, diff, dsplit
, uniq, us
, Plug(..), unplug
) 

where

import Trace

infixl 5 `oemerge`
infixl 5 `unimerge`

msort :: Ord a => [a] -> [a]
msort = oemerges . runs		-- todo: which is better?
	-- mrgsort
	

-- utilities for sorting and merging -----------------------------------

-- find runs
runs :: Ord a => [a] -> [[a]]
runs [] = []
runs [x] = [[x]]
runs (x : xs) = 
    let	rrs @ (r @ (y:_) : rs) = runs xs
    in	if x <= y then (x : r) : rs
	else [x] : rrs


mrgsort :: Ord a => [a] -> [a]
mrgsort [] = []; mrgsort [x] = [x]
mrgsort [x,y] = if x < y then [x,y] else [y,x]
mrgsort xs = 
    let (as, bs) = conquer xs
    in oemerge (mrgsort as) (mrgsort bs)

conquer [] = ([],[])
conquer [x] = ([x], [])
conquer (x : y : zs) = let (as, bs) = conquer zs in (x : as, y : bs)




oemerge :: Ord a => [a] -> [a] -> [a]
-- keeps duplicates
oemerge [] ys = ys; oemerge xs [] = xs
oemerge xxs @ (x : xs) yys @ (y : ys) = 
    if x < y then x : oemerge xs yys else y : oemerge xxs ys


oemerges :: Ord a => [[a]] -> [a]
oemerges [] = []
oemerges [xs] = xs
oemerges [xs,ys] = oemerge xs ys
oemerges xss = 
	let (ass, bss) = conquer xss
	in oemerge (oemerges ass) (oemerges bss)


unimerge :: Ord a => [a] -> [a] -> [a]
-- removes duplicates
unimerge xs [] = xs; unimerge [] ys = ys
unimerge xxs @ (x : xs) yys @ (y : ys) = case compare x y of
	LT -> x : unimerge xs yys
	GT -> y : unimerge xxs ys
	EQ -> x : unimerge xs ys

unimerges :: Ord a => [[a]] -> [a]
-- removes duplicates
unimerges [] = []
unimerges [xs] = xs
unimerges [xs,ys] = unimerge xs ys
unimerges xss = 
	let (ass, bss) = conquer xss
	in unimerge (unimerges ass) (unimerges bss)




uniq :: Ord a => [a] -> [a]
-- arg must be sorted already
uniq [] = []; uniq [x] = [x]
uniq (x : yys @ (y : ys)) 
    = (if x == y then id else (x :)) (uniq yys)

us :: Ord a => [a] -> [a]
us = uniq . msort

diff :: Ord a => [a] -> [a] -> [a]
-- diff xs ys = all x <- xs that are not in ys
-- args must be sorted, without duplicates
diff [] ys = []; diff xs [] = xs
diff xxs @ (x : xs) yys @ (y : ys)
    | x == y    =     diff  xs  ys
    | x < y     = x : diff  xs yys
    | otherwise =     diff xxs  ys
 
dsplit :: Ord a => [a] -> [a] -> ([a],[a])
-- dsplit xs ys = (as, bs) where as = xs intersect ys, bs = xs setminus ys
dsplit [] ys = ([], [])
dsplit xs [] = ([], xs)
dsplit xxs @ (x : xs) yys @ (y : ys)
	| x == y    = let (as, bs) = dsplit xs  ys in (x : as, bs)
	| x <  y    = let (as, bs) = dsplit xs yys in (    as, x : bs)
	| otherwise = let (as, bs) = dsplit xxs ys in (    as, bs)

---------------------------------------------------------------------

best :: Ord a => [a] -> [a]
best = take 1 . reverse . msort

---------------------------------------------------------------------

asc :: Ord b => [(a, b)] -> [(a, b)]
-- show successive maxima, lazily
asc [] = []
asc ((x, c) : xs) = (x, c) : asc [ (x', c') | (x', c') <- xs, c' > c ]

ascWith :: Ord b => (a -> b) -> [a] -> [a]
ascWith f xs = [ x | (x, c) <- asc [ (x, f x) | x <- xs ] ]


-----------------------------------------------------------------------

data Plug a b = Plug a b deriving Show

instance Eq a => Eq (Plug a b) where Plug x _ == Plug y _ = x == y
instance Ord a => Ord (Plug a b) where Plug x _ < Plug y _ = x < y

unplug :: Plug a b -> b; unplug (Plug x y) = y

msortwith :: Ord b => (a -> b) -> [a] -> [a]
msortwith f xs = map unplug . msort $ [ Plug (f x) x | x <- xs ]
