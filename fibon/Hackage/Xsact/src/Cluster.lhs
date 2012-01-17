\documentclass[a4paper]{article}
\pagestyle{myheadings}
\usepackage{haskell}
\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Clustering based on scored pairs}

Given a list of scored pairs, build a hierarchical clustering by
inserting pairs incrementally.

Clustering is performed in two stages
\begin{itemize}
\item Stage one builds a transitive closure, keeping track of ESTs and
the detected SMPairs
\item Stage two, when necessary, builds a hierarchy from each cluster
\end{itemize}

\begin{code}

module Cluster (Cluster,cluster_single,cluster_simple,cluster_all
               ,labels,newick,unigene) where
import Indexed
import EST(EST(..),mkshowlist,mkshowlist',label)
import Pairs(SMPair, Pair(..),direction,elim_bs)
import Data.Set hiding (insert,foldl')
import qualified Data.Set
import Data.List(intersperse,sortBy)
import qualified Data.List as L
import Util(foldl',_log)

type Cluster = (Set EST,[SMPair])

-- | cluster builds a single linkage transitive closure
cluster_single, cluster_simple, cluster_all
    :: Bool -> [EST] -> Int -> [SMPair] -> [Cluster]
cluster_single a_s = cluster a_s ug_merge
cluster_simple a_s = cluster a_s simple_merge
cluster_all a_s es i = cluster a_s all_merge es i . L.map elim_bs
-- cluster_simple i = cluster simple_merge i . map elim_bs
-- cluster_all i = cluster all_merge i . map elim_bs

cluster :: Bool
                                       -> (SMPair -> [SMPair] -> [SMPair])
                                       -> [EST]
                                       -> Int
                                       -> [SMPair]
                                       -> [(Set EST, [SMPair])]
cluster a_s mf es t ps =
    let cs = foldl' (insert mf) [] (L.filter (\x->score x>=t) ps)
        in if a_s then add_singletons es cs else cs

-- add all sequences not in the clusters as singleton clusters
add_singletons :: (Ord a) => [a] -> [(Set a, [a1])] -> [(Set a, [a1])]
add_singletons es cs = let cseqs = foldl' union empty (L.map fst cs)
                           eseqs = foldl' (flip Data.Set.insert) empty es
                           sseqs = eseqs `difference` cseqs -- was: minusSet
                           singleton e = (fromList [e],[])
                           in cs ++ L.map singleton (toList sseqs)

-- keep all pairs, gives an acyclic graph -- i.e. the tree
all_merge :: a -> [a] -> [a]
all_merge p ps = p:ps

-- keep smallest pair, used by (old) unigene behavior
ug_merge :: (Pair a n) => a -> [a] -> [a]
ug_merge p [] = [p]
ug_merge p ps = [head $ sortBy (\x y->compare (score x) (score y)) (p:ps)]

-- keep nothing, useful if you only want the set of ESTs in each cluster
simple_merge :: t -> t1 -> [a]
simple_merge _ _ = []

data Refers a = None [a] | Both a [a] | One a [a] | Two a a [a]

-- inserts an SMPair into a list of Clusters
insert ::  (SMPair -> [SMPair] -> [SMPair]) -> [Cluster] -> SMPair -> [Cluster]
insert mergefn cs p = let
              (e0,e1) = ests p
              in case references cs p of
              -- refs ESTs not in any cluster, create a new cluster
              None cc -> if e0 == e1 then error "cc -- ignore self-refs"
                         else (fromList [e0,e1], mergefn p []) : cc
              -- refs ESTs both in same cluster, just collect the pair
              Both (es,ps) cc -> (es, mergefn p ps) : cc
              -- one EST in a cluster, one without
              One (es,ps) cc -> (Data.Set.insert e es, mergefn p ps) : cc
                  where e = if e0 `member` es then e1 else e0
              -- ESTs in different clusters, join them
              Two (e0s,p0s) (e1s,p1s) cc ->
                  (union e0s e1s, mergefn p (p0s++p1s)) : cc

-- helper for insert
references :: [(Set EST,a)] -> SMPair -> Refers (Set EST,a)
references cs p = let (e0,e1) = ests p
                      in
                      case refs [] cs e0 of
                                 Nothing -> case refs [] cs e1 of
                                      Nothing     -> None cs
                                      Just (c,cc) -> One c cc
                                 Just (c,cc) -> case refs [] [c] e1 of
                                      Just (_c',_) -> Both c cc
                                      Nothing -> case refs [] cc e1 of
                                                   Just (c',cc') -> Two c c' cc'
                                                   Nothing -> One c cc

refs :: (Ord a) => [(Set a, t)] -> [(Set a, t)] -> a
                                    -> Maybe ((Set a, t), [(Set a, t)])
refs _ [] _ = Nothing
refs acc ((es,ps):cs) e
    | e `member` es  = Just ((es,ps), reverse acc ++ cs)
    | otherwise    = refs ((es,ps):acc) cs e

\end{code}

\newpage
\subsection{Generating Simple Output}

Output the labels of the clusters, one cluster on each line.

\begin{code}

labels :: Cluster -> String
labels (ests,_ps) = (concat $ Data.List.intersperse " "
                                $ L.map label (toList ests))

\end{code}

\subsection{Producing Newick-formatted Trees}

Two versions outputting a tree graph in {\em Newick} format.  The
first version produces a tree where branch lengths indicate match
quality and has complete labels.  The second indicates match quality
by the {\em level} of the branch, leading to a prettier tree with
aligned labels.

The {\tt newick2} function is probably the only useful one, and exported
through {\tt newick}.

\begin{code}

data CTree   = Branch SMPair CTree CTree | Leaf EST

newick :: Cluster -> String
newick c = (newick2 $ hierarchy c)++";"

-- | hierarchy constructs the corresponding tree from a cluster
-- (note the reverse sorting)
hierarchy :: Cluster -> CTree
hierarchy (es,ps) = if size es == 1 then (Leaf (unit $ toList es))
                     else (snd . unit . foldl' h_add [] .
                      sortBy (\y x->compare (score x) (score y))) ps
    where unit [x] = x
          unit _   = error "Unit: illegal list size"

-- this is similar to 'insert' above
h_add :: [(Set EST,CTree)] -> SMPair -> [(Set EST,CTree)]
h_add cs p = let (e0,e1) = ests p
             in case cs `references` p of
                None cc   -> (fromList [e0,e1], Branch p (Leaf e0) (Leaf e1)) : cc
                Both c cc -> (c:cc) -- ignore, the pair's redundant
                One (es,br) cc   -> (Data.Set.insert e es, Branch p (Leaf e) br) : cc
                    where e = if e0 `member` es then e1 else e0
                Two (e0,c0) (e1,c1) cc -> (union e0 e1, Branch p c0 c1) : cc

-- build Newick-formatted trees from a hierarchical clustering
newick1, newick2 :: CTree -> String
newick1 (Leaf e) = label e
newick1 (Branch p left right) = "(" ++ newick1 left ++ depth ++ ", "
                         ++ newick1 right ++ depth ++ ")"
    where
    depth = ':':show (1000/fromIntegral (score p))-- from about 1 to 66 (k=15)

-- Adjust so that labels are right-aligned.  Branch height indicate similarity
-- This looks more traditional, and is probably better.
newick2 (Leaf e) =  label e
newick2 c@(Branch _ left right) = "("
                      ++ newick2 left ++ ':':show (depth c - depth left)
                      ++ ", "
                      ++ newick2 right ++ ':':show (depth c - depth right)
                      ++ ")"
    where
    depth (Leaf _) = 0
    depth (Branch p _ _) = 1000.0 / fromIntegral (score p)

\end{code}

\subsection{Outputting UniGene-style clusters}

Unigene clusters consists of a header starting with a hash mark (\#),
followed by the sequences in FASTA format.

We first generate the hierarchical clustering (as in newick), and
calculates a consistent set of directed sequences with a neat
recursion over the tree.  The final (printed) direction is thus
arbitrary, but consistent.

\begin{code}

-- set of forward ESTs and set of reversed ESTs
type DPair = (Set EST,Set EST)

unigene :: Cluster -> String
unigene (es,ps) =
    "# Cluster consisting of " ++
    (if L.null ps then "one sequence\n"
          ++ concatMap (showseq True) (toList es)
     else (show . size) es ++ " sequences,"
          ++ " lowest score is " ++ (show . minimum . L.map score) ps ++ "\n"
          ++ concatMap (showseq True) (toList fwd)
          ++ concatMap (showseq False) (toList rev))
    where
    (fwd,rev) = resolve $ hierarchy (es,ps)

resolve :: CTree -> DPair
resolve (Leaf e) = (fromList [e],empty)
resolve (Branch p l r) = let
          (fwd1,rev1) = resolve l
          (fwd2,rev2) = resolve r
          (e0,e1) = ests p
          d = direction p
          e0f1 = e0 `member` fwd1
          e0f2 = e0 `member` fwd2
          e0r1 = e0 `member` rev1
          e0r2 = e0 `member` rev2
          e1f1 = e1 `member` fwd1
          e1f2 = e1 `member` fwd2
          e1r1 = e1 `member` rev1
          e1r2 = e1 `member` rev2
          codir =  e0f1 && e1f2 || e0f2 && e1f1 || e0r1 && e1r2 || e1r1 && e0r2
          in
          if codir && d || not codir && not d
             then (union fwd1 fwd2, union rev1 rev2)
             else (union fwd1 rev2, union fwd2 rev1)

showseq :: Bool -> EST -> String
showseq d e = ">" ++ label e ++ if d==False -- i.e. reverse
              then " (rev)\n" ++ mkshowlist' (0,len e) e ++"\n"
              else " (fwd)\n" ++ mkshowlist (0,len e) e ++"\n"

\end{code}

\end{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
