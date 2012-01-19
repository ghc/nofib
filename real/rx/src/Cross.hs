module Cross (mmerge, couple, cross, mcross, zipzip, dove) where

mmerge :: [[a]] -> [a]
mmerge xss = mm xss []
	where	mm [] [] = []
		mm [] yss = mm yss []
		mm ([] : xss) yss = mm xss yss
		mm ((x : xs) : xss) yss = x : mm xss (xs : yss)


dove :: [[a]] -> [a]
dove xss = dd 1 xss -- where

dd _ [] = []
dd n (xs : xss) = kzip 0 n (dd (n+1) xss) xs

kzip 0 n as (b : bs) = b : kzip n n as bs
kzip k n (a : as) bs = a : kzip (k-1) n as bs
kzip _ _ [] bs = bs
kzip _ _ as [] = as



couple :: [[a]] -> [[a]]
couple [] = []
couple xss = [ x | (x : _) <- xss ] : couple [ xs | (_ : xs) <- xss ]

cross :: [a] -> [b] -> [(a, b)]
-- product of two infinite lists
cross [] _ = []; cross _ [] = []
cross (x : xs) (y : ys) = 
	(x, y) : mmerge [ [ (x, y') | y' <- ys ]
			, [ (x', y) | x' <- xs ]
			, cross xs ys
			]

mcross :: [[a]] -> [[a]]
-- enumerates the dot product
-- mcross [[x11, x12, ..], [x21, x22, ..], .. , [xn1, xn2, ..]]
--	= [[x11, x21, .., xn1], [.. ], .. ]
mcross xss = [ ys | n <- [0..], ys <- mc n xss] where
    mc :: Int -> [[a]] -> [[a]]
    mc n [xs] = [[xs !! n]]
    mc n (xs : xss) = [ (xs !! m) : ys | m <- [0 .. n], ys <- mc (n-m) xss]


zipzip :: [a] -> [a] -> [a]
zipzip [] ys = ys; zipzip (x : xs) ys = x : zipzip ys xs

