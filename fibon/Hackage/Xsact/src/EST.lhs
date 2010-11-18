\documentclass[a4paper]{article}
\pagestyle{myheadings}
\usepackage{haskell}
\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EST Data Structures}

Implements data structures and functions to perform clustering of
ESTs.

\begin{code}
{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
module EST (EST(..),
            End(..), mkEST   -- only Fasta.lhs
           ,mkshowlist,mkshowlist',label -- Cluster and Pairs
           ,index            -- only Pairs.lhs
           ,indexesOf,tolist -- only Suffix.lhs
           ) where

import Data.Array.Base
import Data.Word
import Indexed
import Gene

\end{code}

\section{Data structures and preparation}

The ESTs are stored in labelled, integer-indexed arrays.
UArray of Word8 is used to conserve memory.

\begin{code}

data End = Three' | Five' | Unknown deriving Eq
data EST = EST !Int String End !(UArray Int Word8)

instance Eq EST where
    (==) (EST n1 _ _ _) (EST n2 _ _ _) = n1 == n2

instance Ord EST where
    compare (EST l1 _ _ _) (EST l2 _ _ _) = compare l1 l2

instance Indexed EST Gene where
    (??) (EST _ _ _ ar) i = fromW8 (ar!i)
    len (EST _ _ _ ar)   = let (a,b) = bounds ar in b-a

-- perhaps use Boyers-Moore/bad character rule (for long words)
indexesOf :: [Word8] -> EST -> [Int]
indexesOf w e@(EST _ _ _ a) = ind 0 w a
    where ind i ws ar
              | i+length ws-1 > len e   = []
              | and [ar!(i+j) == ws!!j | j<-[0..length ws-1]] = i : ind (i+1) ws ar
              | otherwise               = ind (i+1) ws ar

instance Show EST where
    show e = show (label e) ++ "\t" ++ mkshowlist (0, len e) e

-- helper function for showing
mkshowlist,mkshowlist' :: (Int,Int) -> EST -> String
mkshowlist (a,b) e = (concatMap show . map fromW8 . tolist (a,b)) e
mkshowlist' (a,b) e = (concatMap show . revcompl . map fromW8 . tolist (a,b)) e

tolist :: (Int,Int) -> EST -> [Word8]
tolist (a,b) e@(EST _ _ _ ar) = if a<=b
                              then (ar!a) : tolist (a+1,b) e
                              else []

-- extracting the label (very useful)
label :: EST -> String
label (EST _ l _ _) = l

index :: EST -> Int
index (EST i _ _ _) = i

\end{code}

\subsection{Data Construction}

The function {\tt mkEST} builds an {\tt EST} structure from a label
and a list, {\tt mkESTS} turns a list into ESTs, generating labels
automatically.

\begin{code}

_mkESTs :: [[Gene]] -> [EST]
_mkESTs = map mkEST . map (\(i,x,y) -> (i,x,Three',y)) .
         zip3 [(1::Int)..] (map (("s"++) . show) [(1::Int)..])

mkEST :: (Int,String,End,[Gene]) -> EST
mkEST (i,l,e,d) = let a = listArray (0,length d-1) (map toW8 d)
                  in EST i l e a

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\end{document}



