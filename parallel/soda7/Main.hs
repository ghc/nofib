------------------------------------------------------------------
-- Time-stamp: <Sun Jul 21 1996 21:34:33 Stardate: [-31]7844.28 hwloidl>
--
-- Searching in a grid of words for hidden words oriented in any of
-- the 8 possible directions.
--
-- Version 7
-- Hans-Wolfgang Loidl, Kevin Hammond 
-- based on Version 6 of this program (search parallel lines in parallel)
-- by Colin Runciman, May 1984 (Haskell version June 1993)
-- 
------------------------------------------------------------------

module Main(main) where

import Parallel 

data	DIRS = RIGHT | DOWN | DOWNLEFT | UPLEFT
	     | LEFT  | UP   | UPRIGHT  | DOWNRIGHT deriving (Text)

#if defined(GRAN)

main =
     _parGlobal_ 1# 15# 0# 0# (unigrid d) 
	(_parGlobal_ 2# 15# 0# 0# (unigrid dl) 
	  (_parGlobal_ 3# 15# 0# 0# (unigrid ul) (
#if defined(PRINT)
	    let 
	      res = foldr (\ (word,dirs) str -> ((word ++ " -> " ++ (show dirs) ++ "\n") ++ str)) [] (parmap find hidden)
	    in 
	     seq res
	     (appendChan stdout (show res) exit done)
#else
            seq (unilist (parmap find hidden))
            (appendChan stdout "done" exit done)
#endif
     )))
    where
    find word = seq (unilist dirs) (word,dirs)
        where
        dirs = _parGlobal_ 4# 18# 0# 0# (unilist back) (forw ++ back)
        forw = locate word
	       [(r,RIGHT), (d,DOWN), (dl,DOWNLEFT), (ul,UPLEFT)]
        back = locate drow
	       [(r,LEFT), (d,UP), (dl,UPRIGHT), (ul,DOWNRIGHT)]
        drow = reverse word
    r  = grid
    d  = transpose grid
    dl = diagonals grid
    ul = diagonals (reverse grid)

locate word grid = parfilter (any (contains word)) grid

parfilter f [] = []
parfilter f ((g,d):gds) =
	 _parGlobal_ 6# 12# 0# 0# rest
		        (if cond
			   then d : rest
		     	   else rest)
		     	where cond  = f g
			      rest = parfilter f gds

parList [] = []
parList (x:xs) = _parGlobal_ 7# 14# 0# 0# x (seq xs (x:xs))

parmap f [] = []
parmap f (x:xs) = _parGlobal_ 5# 13# 0# 0# fx (seq fxs (fx : fxs))
    where
    fx  = f x
    fxs = parmap f xs

#else  /* !GRAN */

main =
     par (unigrid d) 
	(par (unigrid dl) 
	  (par (unigrid ul) (
#if defined(PRINT)
	    let 
	      res = foldr (\ (word,dirs) str -> ((word ++ " -> " ++ (show dirs) ++ "\n") ++ str)) [] (parmap find hidden)
	    in 
	     seq res
	     (appendChan stdout (show res) exit done)
#else
            seq (unilist (parmap find hidden))
            (appendChan stdout "done" exit done)
     	    -- seq (unilist (parmap find hidden)) (returnPrimIO ())
#endif
     )))
    where
    find word = seq (unilist dirs) (word,dirs)
        where
        dirs = par (unilist back) (forw ++ back)
        forw = locate word
	       [(r,RIGHT), (d,DOWN), (dl,DOWNLEFT), (ul,UPLEFT)]
        back = locate drow
	       [(r,LEFT), (d,UP), (dl,UPRIGHT), (ul,DOWNRIGHT)]
        drow = reverse word
    r  = grid
    d  = transpose grid
    dl = diagonals grid
    ul = diagonals (reverse grid)

locate word grid = parfilter (any (contains word)) grid

parfilter f [] = []
parfilter f ((g,d):gds) =
	 par rest
		        (if cond
			   then d : rest
		     	   else rest)
		     	where cond  = f g
			      rest = parfilter f gds

parList [] = []
parList (x:xs) = par x (seq xs (x:xs))

parmap f [] = []
parmap f (x:xs) = par fx (seq fxs (fx : fxs))
    where
    fx  = f x
    fxs = parmap f xs

#endif   /* GRAN */

unimap :: (a->b) -> [a] -> ()
unimap f [] = ()
unimap f (x:xs) = seq (f x) (unimap f xs)  

unigrid :: [[Char]] -> ()
unigrid = unimap unilist

unilist :: [a] -> ()
unilist [] = ()
unilist (x:xs) = seq x (unilist xs)  

diagonals [r] = map (:[]) r
diagonals (r:rs) = zipinit r ([]:diagonals rs)

zipinit [] ys = ys
zipinit (x:xs) (y:ys) = (x : y) : zipinit xs ys

contains xs ys = any (prefix xs) (suffixes ys)

suffixes [] = []
suffixes xs = xs : suffixes (tail xs) 

prefix [] ys = True
prefix xs [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

grid =
   [['Y', 'I', 'O', 'M', 'R', 'E', 'S', 'K', 'S', 'T'],
    ['A', 'E', 'H', 'Y', 'G', 'E', 'H', 'E', 'D', 'W'],
    ['Z', 'F', 'I', 'A', 'C', 'N', 'I', 'T', 'I', 'A'],
    ['N', 'T', 'O', 'C', 'O', 'M', 'V', 'O', 'O', 'R'],
    ['E', 'R', 'D', 'L', 'O', 'C', 'E', 'N', 'S', 'M'],
    ['Z', 'O', 'U', 'R', 'P', 'S', 'R', 'N', 'D', 'A'],
    ['O', 'Y', 'A', 'S', 'M', 'O', 'Y', 'E', 'D', 'L'],
    ['R', 'N', 'D', 'E', 'N', 'L', 'O', 'A', 'I', 'T'],
    ['F', 'I', 'W', 'I', 'N', 'T', 'E', 'R', 'R', 'C'],
    ['F', 'E', 'Z', 'E', 'E', 'R', 'F', 'T', 'F', 'I'],
    ['I', 'I', 'D', 'T', 'P', 'H', 'U', 'B', 'R', 'L'],
    ['C', 'N', 'O', 'H', 'S', 'G', 'E', 'I', 'O', 'N'],
    ['E', 'G', 'M', 'O', 'P', 'S', 'T', 'A', 'S', 'O'],
    ['T', 'G', 'F', 'F', 'C', 'I', 'S', 'H', 'T', 'H'],
    ['O', 'T', 'B', 'C', 'S', 'S', 'N', 'O', 'W', 'I']]

hidden =
  ["COSY", "SOFT", "WINTER", "SHIVER", "FROZEN", "SNOW",
   "WARM", "HEAT", "COLD",   "FREEZE", "FROST",  "ICE" ]

