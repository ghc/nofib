-- Gene - data definition for genetic material

module Gene where

import System.Random
import Data.Word

data Gene = A | C | G | T | N deriving (Ord,Show,Read)

instance Eq Gene where
		 A == A = True
		 A == _ = False
		 C == C = True
		 C == _ = False
		 G == G = True
		 G == _ = False
		 T == T = True
		 T == _ = False
		 N == _ = False -- N is never treated equal

fromW8 :: Word8 -> Gene
fromW8 w = case w of
		  0 -> A
		  1 -> C
		  2 -> G
		  3 -> T
		  _ -> N

toW8 :: Gene -> Word8
toW8 g = case g of
		A -> 0
		C -> 1
		G -> 2
		T -> 3
		N -> 4

-- uh, why not 'reverse . map compl'?
revcompl :: [Gene] -> [Gene]
revcompl gs = rc [] gs
    where
    rc ss [] = ss
    rc acc (x:xs) = rc (compl x : acc) xs

compl :: Gene -> Gene
compl A = T
compl T = A
compl C = G
compl G = C
compl N = N

instance Random Gene where
	random g = let (c,g') = randomR (0::Int,3) g
		in case c of
		0 -> (A,g')
		1 -> (C,g')
		2 -> (G,g')
		3 -> (T,g')
		_ -> error "can't happen, but compiler warning"
	randomR = error "randomR: not implemented"
