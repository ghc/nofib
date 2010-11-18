% $Id: Fasta.lhs 547 2004-01-26 12:33:16Z ketil $
\documentclass[a4paper]{article}
\pagestyle{myheadings}
\usepackage{haskell}
\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Reading EST Data}

Parses ESTs on FASTA format, and creates corresponding EST data
structures.

\begin{code}

module Fasta (parse, bmfparser, ugparser, FHParser,readDataLower) where

import EST (EST(..), End(..), mkEST)
import Gene -- data structure and revcompl

import Data.Char(isSpace)

readUfile :: FilePath -> IO [EST]
readUfile fn = do
	       inp <- readFile fn
	       return (parse ugparser inp)

\end{code}

\subsection {Parsing}

The FHParser (FASTA header parser) contains functions for extracting
the accession number (or whatever unique identifier you wish, really)
and clone end from the header.

\begin{code}

data FHParser  = FHP {getSeqNo :: String -> String
		     ,getCloneEnd :: String -> End
		     ,getData  :: String -> [Gene]
		     }

ugparser, bmfparser :: FHParser
ugparser = FHP (takeWhile (/=' ') . drop 1)
	       (\s -> case dropIncl "clone_end=" s of
	            ('3':_) -> Three'
	            ('5':_) -> Five'
	            _       -> Unknown)
	       readData

bmfparser = FHP (takeWhile (/=' ') . drop 1)
	        (\s -> case dropIncl "cDNA " s of
	            ('3':_) -> Three'
	            ('5':_) -> Five'
	            _       -> Unknown)
		readDataLower

-- parses a list of clusters, returning the corresponding ESTs
-- fixme: this can cause a stack overflow in some cases -- why?
parse :: FHParser -> String -> [EST]
parse p inp = zipWith (readEST p) [1..]
	      ((ests . filter (\x->head x/='#') . filter (/="") . lines) inp)

\end{code}

\subsection{Processing}

The functions that actually do the work.

\begin{code}

-- take a list of lines, partition it into a list of ESTs
ests :: [String] -> [[String]]
ests [] = []
ests inp = let (xs,rest) = splitWhen '>' inp
		   in xs : ests rest

-- split a list of lines when a line starts with x
splitWhen :: Char -> [String] -> ([String],[String])
splitWhen _ [] = error "splitWhen can't split an empty list"
splitWhen x (l:ls)
    | head l /= x =
	error ("Incorrect data format (expecting "++show x++"):"++ show l)
    | otherwise   = (l:first, rest)
    where
    (first, rest) = mysplit [] ls
    mysplit acc [] = (reverse acc, [])
    mysplit acc ("":ms) = mysplit acc ms
    mysplit acc (m:ms) = if head m == x then (reverse acc, m:ms)
			 else mysplit (m:acc) ms

-- use a parser to extract information, and produce ESTs in both directions
readEST :: FHParser -> Int -> [String] -> EST
readEST _ _ []        = error "readEST cannot read empty data"
readEST p i (hd:seqs) =
    if seqno==seqno && end==end
    then mkEST (i,seqno, end, (getData p) $ concat seqs)
    else error "Can't happen (it's just for strictness, dummy!)"
    where seqno = (getSeqNo p) hd
	  end = (getCloneEnd p) hd

-- readData - read sequence data from input, ignoring whitespace (\n)
-- belongs in the Gene module, perhaps?
-- Note that sometimes lower case means masked/clipped
readDataLower, readData :: String -> [Gene]
readDataLower = map read1 . filter (not.isSpace)
  where read1 x = case x of
		 'a' -> A
		 'c' -> C
		 'g' -> G
		 't' -> T
		 'A' -> A
		 'C' -> C
		 'G' -> G
		 'T' -> T
		 _ -> N

readData = map read1 . filter (not.isSpace)
  where read1 x = case x of
		 'A' -> A
		 'C' -> C
		 'G' -> G
		 'T' -> T
		 _ -> N

dropUntil :: ([a] -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil p xs = if p xs then xs else dropUntil p (tail xs)

dropIncl :: Eq a => [a] -> [a] -> [a]
dropIncl s = drop (length s) . dropUntil (==s)

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\end{document}



