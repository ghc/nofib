\documentclass[a4paper]{article}
\pagestyle{myheadings}
\usepackage{haskell}
\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Word Map}

A Word Map of a data set, is a map that for a given word of length
less than some k, returns all positions of the word in the data set.

Implemented by storing all occurrences of k-words in a Map.

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
module WordMap (WordMap,Key,genkeys,genkeysN,list2keys,
                mkWordMap,get_k,wmlookup,
                n2i,i2n,i2s,shift_left,shift_right) where

import Gene
import Suffix (Suffix(..),Dir(..))
import Indexed
import EST (EST)
import Util(foldl')

import qualified Data.Map as Map hiding (map, filter)

type Key = Integer
type WordMap = Map.Map Key [Suffix]
type WordMapper = Int -> WordMap -> EST -> WordMap

\end{code}
\newpage
\subsection{Building the WordMap}

The suffix map is built by calculating hash values incrementally as we
traverse the ESTs.  Several versions are provided, with varying
properties.

Note that the flip is needed to make sure (++) prepends, rather than
appends; this is faster.

\begin{code}

mkWordMap :: Int -> [EST] -> WordMap
mkWordMap _k = foldl' (mkWordMap1 _k) Map.empty

-- ignore any Ns, conserving memory and more robust traversals
mkWordMap1 :: WordMapper
mkWordMap1 k sm e = (foldr . uncurry . Map.insertWith) (flip (++)) sm $
                    map (\(p,w) -> (w,[Suf Fwd e p])) $ genkeys k 0 e

-- genkeys takes word size, position, EST and generates the list of Pos/Keys
genkeys :: Int -> Int -> EST -> [(Int,Key)]
genkeys k p e | len e < p+k-1 = []
              | null (filter isN w0) = genkeys' k (n2i w0) p e
              | otherwise            = genkeys k (p+1) e
    where w0 = map (e ??) [p..p+k-1]

-- genkeys' takes word size, current word and position, and generates
-- Keys until an N is encountered
genkeys' :: Int -> Key -> Int -> EST -> [(Int, Key)]
genkeys' k i p e
    | p > len e-k = [(p,i)]
    | otherwise     = if isN (e ?? (p+k)) then (p,i) : genkeys k (p+k+1) e
                      else let i' = (i `mod` 5^(k-1))*5+val (e ?? (p+k)) + 5^k
                               in (p,i) : genkeys' k i' (p+1) e

isN :: Gene -> Bool
isN = not . (flip elem) [A,C,G,T]

-- genkeysN is like genkeys, but includes N-words
genkeysN :: Int -> Int -> EST -> [(Int,Key)]
genkeysN k p e | len e < p+k-1 = []
               | otherwise = genkeysN' k (n2i w0) p e
    where w0 = map (e ??) [p..p+k-1]

genkeysN' :: (Indexed a Gene) => Int -> Key -> Int -> a -> [(Int, Key)]
genkeysN' k i p e
        | p > len e-k = [(p,i)]
        | otherwise   = let i' = (i `mod` 5^(k-1))*5+val (e ?? (p+k)) + 5^k
                             in (p,i) : genkeysN' k i' (p+1) e

-- inefficient, but should work
list2keys :: Int -> [Gene] -> [Key]
list2keys k gs | length gs < k = []
               | otherwise     = n2i (take k gs) : list2keys k (tail gs)

\end{code}

\newpage

\subsection{Lookup function}

This function is basically equivalent to {\tt fromJust \$ lookupFM},
but provides better error messages.

\begin{code}

wmlookup :: WordMap -> Key -> [Suffix]
wmlookup wm k = case (flip Map.lookup) wm k of
                          Just x -> x
                          Nothing -> error str
    where str = "Lookup failed, key="++show k++", word="++concatMap show (i2n k)

\end{code}

\subsection{(De-)Constructing Keys}

Encoding and decoding  hash values (Integer) to/from strings of
nucleotides.

\begin{code}

-- | given a key, calculate the key corresponding to the revcompl word
-- revcomplKey :: Key -> Key
-- revcomplKey = n2i . revcompl . i2n  -- inefficient

val :: Gene -> Key
val g = case g of { A -> 0; C -> 1; G -> 2; T -> 3; N -> 4;
                    -- _ -> error ("Illegal Gene"++show g)
                  }

unval :: Key -> Gene
unval i = case i of {0 -> A; 1 -> C; 2 -> G; 3 -> T; 4 -> N;
                     _ -> error ("Illegal value "++show i)}

n2i :: [Gene] -> Key
n2i = n2i' 1 -- "stop bit"
    where n2i' acc (g:gs) = n2i' (5*acc + val g) gs
          n2i' acc []     = acc

i2n :: Key -> [Gene]
i2n = reverse . i2n'
    where i2n' i = if i > 1
                   then let (q,r) = i `divMod` 5 in unval r : i2n' q
                   else []

-- very useful for debugging
i2s :: Key -> String
i2s = concatMap show . i2n

-- for quickCheck
i2n_prop :: Key -> Bool
i2n_prop i  = (n2i . i2n)  i == i
n2i_prop :: [Gene] -> Bool
n2i_prop gs = (i2n . n2i) gs == gs

\end{code}

\subsection{Graph Edge Properties}

In a splice graph, vertices are $k-1$-words, and edges link words that
are subsequent in the data set.  The WordMap is an equivalent way of
representing this graph, each $k$-word representing an edge in the
graph.

Thus, for a $k-1$-word, we can calculate its in- and outdegree by
searching the WordMap.

(Possibly, this belongs in SpliceGraph?)

\begin{code}

-- extract the k value
get_k :: WordMap -> Int
get_k wm = length $ i2n $ fst $ head $ Map.assocs wm

-- shifting a word (or really its key) and pre/appending a new nucleotide
shift_left, shift_right :: Int -> Key -> Gene -> Key
shift_left k w g  = 5 * (w `mod` 5^(k-1)) + val g + 5^k
shift_right k w g = (w-5^k) `div` 5 + 5^(k-1)*val g + 5^k

-- find edges from a node
edges_left, edges_right :: WordMap -> Key -> [Key]
edges_right = edges True
edges_left  = edges False

edges :: (Eq a) => Bool -> Map.Map Key a -> Key -> [Key]
edges dir wm w =
    let k  = length $ i2n $ fst $ head $ Map.assocs wm
        ns = map (if dir == True then ((shift_left k) (shift_right k w N))
                  else ((shift_right k) (shift_left k w N))) [A,C,G,T,N]
        in map fst $ filter ((/=Nothing).snd) $ map (\x->(x,(flip Map.lookup) wm x)) ns

indegree, outdegree :: WordMap -> Key -> Int
indegree = undefined
outdegree = undefined

\end{code}

\subsection{Testing}
\begin{code}

-- let test = unlines $ (map (\(x,y)->concatMap show (i2n x)++" "++show y) .Map.assocs . mkWordMap 2 . EST.mkEST) (1,"foo",EST.Five',[A,G,G,C,T,T,A,G,G])

\end{code}
\end{document}
