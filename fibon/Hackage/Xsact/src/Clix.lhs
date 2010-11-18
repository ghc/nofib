\documentclass[a4paper]{article}
\usepackage{haskell}
\pagestyle{myheadings}
\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Not building the Suffix List}

Why not calculate cliques directly, instead of going way of the suffix
list/array?  Here goes:

\begin{code}

module Clix where

import Suffix (Suffix)
import Indexed
import Gene

-- | clix calculates the cliques resulting from sorting to a specific depth 
clix :: Int -> [[Suffix]] -> [[Suffix]]
clix i = clix' 0 i 

-- | takes current offset and target offset and sorts the suffixes 
-- to the target depth.  
-- fixme: seems to cause a stack overflow if cliques become large(?)
clix' :: Int -> Int -> [[Suffix]] -> [[Suffix]]
clix' cur tgt (s:ss) = 
    if cur == tgt then (s:ss) 
       else  (clix' (cur+1) tgt (filter (\x -> length x > 1) [as,cs,gs,ts])) 
		 ++ clix' cur tgt ss
    where
    (as,cs,gs,ts) = clix1' cur ([],[],[],[]) s
clix' _ _ [] = []

-- split the list according to nucleotide in position p
clix1' p (a,c,g,t) [] = (a,c,g,t)
clix1' p  (a,c,g,t) (x:xs)
    | p > len x = clix1' p (a,c,g,t) xs
    | otherwise = case (x ?? p) of
			       A -> clix1' p (x:a,c,g,t) xs
			       C -> clix1' p (a,x:c,g,t) xs
			       G -> clix1' p (a,c,x:g,t) xs
			       T -> clix1' p (a,c,g,x:t) xs
			       _ -> clix1' p (a,c,g,t) xs

\end{code}
\end{document}
