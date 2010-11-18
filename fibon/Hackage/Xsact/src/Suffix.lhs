\documentclass[a4paper]{article}
\usepackage{haskell}
\pagestyle{myheadings}
\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Building the Suffix List}

This module defines the {\tt Suffix} data type, as well as operations
constructing the set of cliques.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
module Suffix (Suffix(..),Dir(..)
              ,suffixesBy -- Xsact
              ,cmp        -- Pairs
              ,getEST, getPos -- SpliceGraph
              ,getDir
              ) where
import EST(EST(..), indexesOf, label)
import Gene
import Indexed

data Dir = Fwd | Rev deriving (Eq,Show)
data Suffix = Suf !Dir !EST !Int deriving Eq   -- EST and offset

instance Indexed Suffix Gene where
    (??) (Suf Fwd e ix) i  = e??(ix+i)
    (??) (Suf Rev e ix) i = compl (e??(ix-i))
    len (Suf Fwd e ix)   = len e - ix
    len (Suf Rev _ ix)   = ix

instance Show Suffix where
    show (Suf Fwd e i) = label e
                     ++ "["++show i++"]"
--                   ++ (concatMap show . map fromW8) (tolist (i, len e) e)
    show (Suf Rev e i) = "R" ++ label e
                     ++ "["++show i++"]"
--                   ++ (concatMap show . revcompl . map fromW8)
--                          (tolist (0, i) e)

\end{code}

\newpage

\subsection{Some Useful Helper Functions}

Clustering of ESTs require comparing suffixes based on ESTs, which in
turn are uniquely identified by their labels.

\begin{code}

-- generate all suffixes from an EST
suffixes :: EST -> [Suffix]
suffixes e  = map (Suf Fwd e) [0..len e] ++ map (Suf Rev e) [0..len e]

-- extract all suffixes that are prefixed by the given word
suffixesBy :: [Gene] -> EST -> [Suffix]
suffixesBy prefix e = let
        p  = map toW8 prefix
        p' = reverse $ map (toW8 . compl) prefix
        in
        map (Suf Fwd e) (indexesOf p e) ++
        map (Suf Rev e) (map (+(length p'-1)) (indexesOf p' e))

-- construct a label from the EST label and bracketed offset
slabel :: Suffix -> String
slabel (Suf Fwd e pos) = label e ++ "[" ++ show pos ++ "]"
slabel (Suf Rev e pos) = label e ++ "[-" ++ show pos ++ "]"

-- cmp compares two suffixes based on the EST they reference
cmp :: Suffix -> Suffix -> Ordering
cmp (Suf _ l1 _) (Suf _ l2 _) = compare l1 l2

getEST :: Suffix -> EST
getEST (Suf _d e _p) = e

getPos :: Suffix -> Int
getPos (Suf _d _e p) = p

getDir :: Suffix -> Dir
getDir (Suf d _e _p) = d

\end{code}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\end{document}