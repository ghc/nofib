\documentclass[a4paper]{article}
\usepackage{haskell}
\pagestyle{myheadings}
\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Splice Graph Construction}

This module builds splice graphs from {\tt WordMap}s.  Each word
$w_1w_2w_3...w_n$ in the map represents an edge from $w_1w_2..w_{n-1}$
to $w_2w_3..w_n$.  It also generates the greedy paths representing
consensus sequences.

\begin{code}

module SpliceGraph (Path, greedy_paths, path_sequence, show_graph) where

import WordMap (WordMap,Key,wmlookup,get_k,genkeysN,
                n2i,i2n,i2s,shift_left,shift_right,genkeys)
import qualified Data.Map as Map hiding (filter, map)
import Data.List (nub, sort, elem) -- (sort,nub,maximumBy,groupBy,sortBy)
import Data.Maybe(fromJust,maybeToList,listToMaybe)
import Data.Set hiding (filter,map,null)
import EST(EST,label)
import Gene
import Indexed
import Suffix(Suffix,getEST,getPos)
import Util  (foldl', sortOn)

type Branch = Key
type Path = [Key]

-- foo :: String
-- foo = "let foo be a string with \n in it"

-- this definition is probably faster than wmlookup:
-- mlookup fm = fromJust . lookupFM
mlookup :: WordMap -> Key -> [Suffix]
mlookup = wmlookup

{- shadows import -}
_log :: t -> ()
_log _ = ()

\end{code}
\subsection{Greedy (and less greedy) paths}

We find the remaining greediest path by starting with the most common
edge, and traversing in each direction.  This is iterated, until no
edge heavy enough remains.

\begin{code}

-- find the heaviest (consensus) paths
greedy_paths :: Int
                                            -> WordMap
                                            -> [(String, Int, Int, Path)]
greedy_paths k wm = greedy_paths' k wm empty

greedy_paths' :: Int -> WordMap -> Set Key -> [(String,Int,Int,Path)]
greedy_paths' minw wm exc =
                       let (w0,weight) = heaviest_except exc wm
                           path = greedy_path wm w0
                           exc' = Data.Set.union exc $ fromList path
                           in if weight >= minw && weight > 0
                              then (i2s w0,weight,score_path wm path,path)
                                       : greedy_paths' minw wm exc'
                              else []


-- only count unique occurrences
score_path :: WordMap -> Path -> Int
score_path wm = foldr (+) 0 . map (fst . count_unique . fj . (flip Map.lookup) wm)
    where fj (Just xs) = xs
          fj Nothing    = []

\end{code}

Finding the heaviest edge in the word map {\em not} already in a path.
The ''used'' edges are stored in a set of keys.

\begin{code}

-- todo: perhaps even stricter repeat avoidance?  square it?
heaviest_except :: Set Key -> WordMap -> (Key,Int)
heaviest_except exceptions = h' (error "h_e undef",0) . (Map.assocs)
    where h' (current,weight) ((x,ss):rest) =
              if (not $ x `member` exceptions) && count_u ss > weight
                 then h' (x,count_u ss) rest
                 else h' (current,weight) rest
          h' (current,weight) [] = (current,weight)
          count_u = (\(x,y)->x `div` y) . count_unique

-- return unique count (number of matching sequences)
-- and max repeats in one sequence
count_unique :: [Suffix] -> (Int,Int)
count_unique ss = let count = foldl' uq Map.empty ss
                      in (Map.size count, maxFM count)
    where
    uq counts s = case (flip Map.lookup) counts e of
                  Nothing -> Map.insert e 1 counts
                  Just x  -> Map.insert e (x+1) counts
        where e = getEST s
    maxFM = maximum . map snd . (Map.assocs)

\end{code}

\newpage
\subsection{Calculating a greedy path}

Calculating the greediest path in a WordMap, given a Key as a
starting point.

\begin{code}

-- helper function
mkseqpos :: Suffix -> (EST, Int)
mkseqpos s = (getEST s, getPos s)

greedy_path :: WordMap -> Key -> Path
greedy_path wm w0 =
    let sm0 = Map.fromList $ map mkseqpos $ mlookup wm w0
        wm0 = Map.mapWithKey (\_ _ -> get_k wm) sm0
        (lks,rks) = mkpath wm (sm0,wm0,sm0) ([w0],[w0])
        -- this looks funny, but is correct; mkpath outputs reversed paths
        path = lks ++ drop 1 (reverse rks)
        in
        path

\end{code}

\subsection{The pathfinding algorithm}

We define the SeqMap to keep track of used positions in sequences, to
aid us in chosing branches in the graph.  Among other things, this
helps us to avoid looping.  (This can easily happen if you have two
sequences "v...w" and "w...v").

\begin{code}

-- data SeqMap = SM {left, weight, right :: Map.Map EST Int }
type SeqMap = (Map.Map EST Int,Map.Map EST Int,Map.Map EST Int)
left :: (t, t1, t2) -> t
left (l,_,_) = l
right :: (t, t1, t2) -> t2
right (_,_,r) = r
weight :: (t, t1, t2) -> t1
weight (_,w,_) = w

data Direction = R | L deriving Eq
data Alt = Alt { alt_key :: Key, score :: Int,
                 support, follow_on, all_compat :: [Suffix] }
           deriving (Eq,Show)

alt :: Alt
-- default value (hacked to avoid warnings about undefined)
alt = Alt (error "Alt with undef. alt_key") (error "Alt with undef. score")
      (error "Alt with undef. support") (error "Alt with undef. follow_on")
      (error "Alt with undef. all_compat")

\end{code}

The actual algorithm for finding a path.  Determines all alternatives,
and chooses the best one.  When no more alternatives can be found
(i.e. all sequences end or are N), "extend" is called to try a bit
harder, or to finish the job.

This can probably be optimized quite a bit (50\%) by retaining unused
Alt's instead of recalculating them each iteration.

\begin{code}
mkpath :: WordMap -> SeqMap -> (Path,Path) -> (Path,Path)
mkpath wm sm (left,right) = let
        k = get_k wm
        lws = filter nonE $
              map (mkAlt wm sm L . shift_right  k (head left))  [A,C,G,T]
        rws = filter nonE $
              map (mkAlt wm sm R . shift_left k (head right)) [A,C,G,T]
        nonE = not . null . all_compat
        continue (Left k) =
            mkpath wm (updateSM sm (Left k)) (alt_key k:left,right)
        continue (Right k) =
            mkpath wm (updateSM sm (Right k)) (left,alt_key k:right)
        in _log (lws `seq` rws `seq` "ALTS:\nleft: "++
           show (map (i2s . alt_key) lws) ++ "\nright: "
        ++ show (map (i2s . alt_key) rws)) `seq`
           case (lws,rws) of
          ([],[]) -> extend wm sm (left,right)
          ([x],[]) -> continue (Left x) -- what if they already exist?
          ([],[x]) -> continue (Right x)
          (_,_)  -> continue (bestbranch (lws,rws))

-- find the best branch given the seqmap and the wordmap
bestbranch :: ([Alt],[Alt]) -> Either Alt Alt
bestbranch (l,[]) = Left (best l)
bestbranch ([],r) = Right (best r)
bestbranch (l,r)  = let (bl,br) = (best l, best r)
                    in _log (bl `seq` br `seq` "bb: = "++show (bl,br)) `seq`
                       if best [bl,br] == bl then Left bl else Right br

best :: [Alt] -> Alt
best = head . sortOn (negate . score)

--           _log ("picking best of: "++show (map alt_key as)) `seq`
--     head $ sortOn (negate . length . all_compat) as''
--     where
--     as' = sortOn (negate . length . follow_on) as
--     as'' = takeWhile (\a -> (length $ follow_on $ head as')
--                    == (length $ follow_on a)) as'

\end{code}

Fixme: if the best branch has $>$ minw sequences in follow\_on, take it
else take highest follow\_on supported by compats (exchg rate?)

\subsection{Making Alts}

Constructing the Alt data structure.  This contains the necessary
information for determining branch choices.

\begin{code}

mkAlt :: WordMap -> SeqMap -> Direction -> Key -> Alt
mkAlt wm sm d k = -- _log (with_follow `seq` "mkAlt:"++show with_follow) `seq`
    calc_score sm with_follow
    where
    lookup = concat . maybeToList . (flip Map.lookup) wm
    with_support = alt { alt_key = k, support = lookup k }
    -- find all sequences extend l/r with gap
    with_compat = with_support { all_compat = filter (isCompat sm d)
                                 (support with_support) }
    -- find all sequences that extend l/r with exactly one
    with_follow = with_compat { follow_on = filter follows
                                (support with_compat) }
    --
    follows s = let (e,p) = mkseqpos s
                    l = (flip Map.lookup) (left sm) e
                    r = (flip Map.lookup) (right sm) e
               in case d of
                   L -> l /= Nothing && fromJust l == p+1
                   R -> r /= Nothing && fromJust r == p-1

isCompat :: SeqMap -> Direction -> Suffix -> Bool
isCompat sm d s = let (e,p) = mkseqpos s
                      l = (flip Map.lookup) (left sm) e
                      r = (flip Map.lookup) (right sm) e
        in case d of
           L -> l == Nothing || fromJust l > p
           R -> r == Nothing || fromJust r < p

-- one possible score calculation
calc_score :: SeqMap -> Alt -> Alt
calc_score (_l,w,_r) alt = alt { score = sum $ getweights $ follow_on alt}
    where fj Nothing  = 0
          fj (Just w) = w
          getweights =
              map (\x -> (max ((fj . (flip Map.lookup) w . getEST) x) 42))
--                 / dist x l r)
--        dist x l r = min (distL l x) (distR r x)
--        distL l x =

\end{code}

\subsection{Extending paths}

When no more matches can be found, see if we can extend it
with the remains of prematurely terminated sequences.

Todo: should probably use make Alts to determine the best branch...

\begin{code}

extend :: WordMap -> SeqMap -> ([Key],[Key]) -> ([Key],[Key])
extend wm sm (l:ls,r:rs) = _log ("extend: "++i2s l++","++i2s r) `seq`
    let ssl = filter (isEdge sm L) $ filter (not.isStart) $ mlookup wm l
        ssr = filter (isEdge sm R) $ filter (not.isEnd) $ mlookup wm r
        next_l = longest $ map words_left ssl
        next_r = longest $ map words_right ssr
    in _log (show ((ssl,ssr),(sh next_l,sh next_r))) `seq`
    case (next_l,next_r) of
        (Nothing,Nothing) -> _log (show $ (Map.assocs) $ weight sm) `seq`
                             (l:ls,r:rs)
        (Nothing,Just (e,x))  -> mkpath wm
                      (updateSM sm (Right $ mkAlt wm sm R $ snd $ head x))
                      (l:ls, reverse (mkNs wm R e ssr r (snd $ head x)) ++ rs)
        (Just (e,x),_) -> mkpath wm
                      (updateSM sm (Left $ mkAlt wm sm L $ snd $ last x))
                      (mkNs wm L e ssl (snd $ last x) l ++ ls, r:rs)
    where
    -- pick the seq. longest after the next word
    k = get_k wm
    -- what is the best key to go on with?  We choose the one from the
    -- sequence with most remaining keys, but we should probably also
    -- consider keys with support from many sequences.
    longest :: [(EST,[(Int,Key)])] -> Maybe (EST,[(Int,Key)])
    longest = listToMaybe . sortOn (negate.length.snd) . filter (not.null.snd)
    -- generate all words in a sequence to the right (left) of a suffix
    words_right, words_left :: Suffix -> (EST,[(Int,Key)])
    words_right s = (getEST s, genkeys k (1+getPos s) (getEST s))
    words_left s  = (getEST s, takeWhile ((getPos s >).fst) $
                               genkeys k 0 (getEST s))
    -- test for start or end of a suffix
    isStart = (k >) . getPos
    isEnd   = (2*k >) . len
    -- display a position (int) and key pair
    sh Nothing = []
    sh (Just (e,xs)) = map (\(p,key)->(label e,p,i2s key)) xs
extend _  _   _ = error "Cannot extend an empty path"

isEdge :: SeqMap -> Direction -> Suffix -> Bool
isEdge sm L s = (flip Map.lookup) (left sm) (getEST s) == Just (getPos s)
isEdge sm R s = (flip Map.lookup) (right sm) (getEST s) == Just (getPos s)

\end{code}

Make a set of intermediate keys from left to right key, taken from the
sequence chosen to "extend" the path.

(This is more than a bit ugly!)

\begin{code}

mkNs :: WordMap -> Direction -> EST -> [Suffix] -> Key -> Key -> [Key]
mkNs wm d est ss lk rk = let
    k = length $ i2n lk
    thisESTpos = getPos . head . filter ((==est).getEST)
    in _log ("mkNs: "++show (thisESTpos (mlookup wm lk),thisESTpos ss,thisESTpos (mlookup wm rk))) `seq`
    if d == L
    then map snd $ takeWhile ((thisESTpos ss>=).fst)
             $ genkeysN k (thisESTpos (mlookup wm lk)) est
    else map snd $ takeWhile ((thisESTpos (mlookup wm rk)>=).fst)
             $ genkeysN k (thisESTpos ss) est

\end{code}

Update the sequence map to incorporate the chosen alternative.
Using intermediate lists $\rightarrow$ opportunity for optimization.
For all sequences already in the SM, update left bounds.
For all sequences *not* in the SM, add it to both left and right.

\begin{code}

-- todo: group suffixes by EST, choose most compatible (closest) position
-- perhaps: do not add sequences to SM for known repeats?
updateSM :: SeqMap -> Either Alt Alt -> SeqMap
updateSM sm (Left alt) = foldl' add1 sm ps
     where
     ps = map (\s -> (getEST s, getPos s)) (all_compat alt)
     add1 (l,w,r) (e,p) = case (flip Map.lookup) l e of
             Nothing -> (Map.insert e p l, Map.insert e k w, Map.insert e p r)
             Just pos -> (Map.insert e p l, Map.insertWith (+) e (min (pos-p) k) w, r)
         where k = length $ i2n $ alt_key alt
updateSM sm (Right alt) = foldl' add1 sm ps
     where
     ps = map (\s -> (getEST s, getPos s)) (all_compat alt)
     add1 (l,w,r) (e,p) = case (flip Map.lookup) r e of
             Nothing -> (Map.insert e p l, Map.insert e k w, Map.insert e p r)
             Just pos -> (l, Map.insertWith (+) e (min (p-pos) k) w, Map.insert e p r)
         where k = length $ i2n $ alt_key alt

-- convert a path of keys to the corresponding sequence
path_sequence :: Path -> [Gene]
path_sequence (x:xs) = -- _log (show $ map i2s (x:xs)) `seq`
                       i2n x ++ map (last . i2n) xs
path_sequence [] = error "No path with no keys"

\end{code}

\newpage
\section{Graph Output}

Write output suitable for GraphViz to produce nice pictures from.

We need to collapse nodes with indegree and outdegree of one, in order
to avoid excessive size of the graph (although GraphViz handles them
fine).

\begin{code}

show_graph :: Int -> WordMap -> [[Branch]] -> String
show_graph minw wm bs = comments ++ header ++
                   (unlines $ print_nodes k nodepairs) ++
                   (unlines $ map (print_edge minw bs wm) nodepairs)
                   ++ footer
    where header = "digraph splicegraph {\n"++
--                 "node [ shape = circle, label = \"\"];\n" ++
--                 "node [ shape = box ];\n" ++
--                 "size = \"8,5\";\n" ++
                   "rankdir = LR;\n" ++
                   "orientation = land;\n"
          footer = "}\n"
          comments = "// Produced by xtract\n"
          nodepairs = optimize $ node2str $ map edge2nodes $ edges wm
          k = get_k wm

-- construct the pair of nodes from an edge
edge2nodes :: [Gene] -> ([Gene],[Gene])
edge2nodes gs = (take (length gs-1) gs, tail gs)

-- find all the edges in a graph
edges :: WordMap -> [[Gene]]
edges wm = map (i2n . fst) $ (Map.assocs) wm

node2str :: (Show a, Show a1) => [([a], [a1])] -> [(String, String)]
node2str = map (\(x,y)->(concatMap show x,concatMap show y))


-- producing a Graphviz-formatted node list
print_nodes :: Int -> [(String,String)] -> [String]
print_nodes k ns = map shownode $ unique ns
    where shownode x = x ++ " [ label=\""
                       ++ (case indegree ns x of
                           0 -> x --  ++ "\" color=\"green"
                           _ -> drop (k-2) x)
                       ++ "\" shape=\"box\" ]"
          unique = nub . sort . concatUnzip
          concatUnzip ((a,b):abs) = a:b: concatUnzip abs
          concatUnzip [] = []

-- producing a GraphViz-formatted edge
print_edge :: Int -> [[Branch]] -> WordMap -> (String,String) -> String
print_edge minw bs wm (n1,n2) = n1 ++ " -> " ++ n2 ++ " [ "
--                   ++ "label = \""++show (last n2) ++ "\"" ++
                     ++ (let x = length $ mlookup wm key in
                         if x == 1 then " style=\"dotted\""
                         else if x > minw then " style=\"bold\"" else "")
--                   ++ (if key `elem` bs then " label=\""++show (fromJust $ elemIndex key bs) ++"\"" else "")
                     ++ " color=\""
                     ++ (if key `elem` (bs!!0) then "red\""
{-                       else if key `elem` (bs!!1) then "blue\""
                         else if key `elem` (bs!!2) then "green\""
--                       else if key `elem` (bs!!3) then "magenta\""
-}                       else "black\"")
                     ++ "]; // "
--                   ++ (show $ fromJust $ (flip Map.lookup) wm key)
--                    where key = i2s (head n1:n2)
    where k = get_k wm
          key = n2i $ map (read.(:[]))
                (reverse ((head $ drop (k-2) n2) : take (k-1) (reverse n1)))

type EdgeMap a = Map.Map [a] [[a]]
type Edge a = ([a],[a])

-- WARNING: won't work on [Gene], because of the bone-headed Eq instance!
-- remove singleton edges
optimize :: (Show a, Eq a, Ord a) => [Edge a] -> [Edge a]
optimize es = opt k (fwd_edges, rev_edges)
    where
    k = length $ fst $ head es
    -- explicit map of forward edges and reverse edges
    fwd_edges = (foldr . uncurry . Map.insertWith) (flip (++)) Map.empty $ map ( \(x,y) -> (x,[y])) es
    rev_edges = (foldr . uncurry . Map.insertWith) (flip (++)) Map.empty $ map ( \(x,y) -> (y,[x])) es

opt :: (Show a, Ord a) => Int -> (EdgeMap a,EdgeMap a) -> [Edge a]
opt l edges  =
    -- extract edges with no "external" connections
    let sgls = [(n0,n1) | (n0,[n1]) <- (Map.assocs) $ fst edges
               , (flip Map.lookup) (snd edges) n1 == Just [n0]]
        in case sgls of
            ((e0,e1):_) ->
                    let new = e0 ++ drop (l-1) e1
                        -- remove (e0,e1)
                        f = (flip Map.delete) (fst edges) e0
                        r = (flip Map.delete) (snd edges) e1
                        -- more efficient to look up e0 and e1 first:
                        i' = (flip Map.lookup) r e0
                        incoming = case i' of {Just x  -> x; Nothing -> []}
                        o' = (flip Map.lookup) f e1
                        outgoing = case o' of {Just x  -> x; Nothing -> []}
                        -- replace (x,e0) with (x,new)
                        f' = replace f e0 new incoming
                        f'' = (flip Map.delete) f' e1
                        f''' = if outgoing == [] then (flip Map.delete) f'' new
                               else Map.insert new outgoing f''
                        -- replace (e1,y) with (new,y)
                        r' = replace r e1 new outgoing
                        r'' = (flip Map.delete) r' e0
                        r''' = if incoming==[] then (flip Map.delete) r'' new
                               else Map.insert new incoming r''
                        in -- _log (show sgls ++ " " ++ show new ++ " " ++ " " ++ (show $ map ((Map.assocs))  [f''',r'''])) `seq`
                           opt l (f''',r''')
            [] -> -- _log ("done: "++show (map (Map.assocs) [fst edges, snd edges])) `seq`
                  e2l edges

-- perform replacement
replace :: (Show a,Ord a) => EdgeMap a -> [a] -> [a] -> [[a]] -> EdgeMap a
replace fm _ _ [] = fm
replace fm old new (l:ls) = -- _log (show ("replace:",old,new,(l:ls))) `seq`
                            let ts = (flip Map.lookup) fm l
                                rep1 old new x = if x == old then new else x
                                fm' = Map.insert l (map (rep1 old new)
                                                     $ fromJust ts) fm
                                in -- _log (show ts) `seq`
                                   if ts == Nothing
                                   then replace fm old new ls
                                   else replace fm' old new ls
-- turn the FMs to a list
e2l :: (EdgeMap a,EdgeMap a) -> [Edge a]
e2l fms = let es = (Map.assocs) $ fst fms
              in concatMap (\(s,ts) -> map ((,) s) ts) es

indegree, outdegree :: (Eq a) => [Edge a] -> [a] -> Int
indegree  ns n = length $ filter ((==n).snd) ns
outdegree ns n = length $ filter ((==n).fst) ns

-- for a node, calculate indegree/outdegree
indeg, outdeg :: (Ord a) => (EdgeMap a, EdgeMap a)
                    -> [a] -> Int
indeg  ns n = case (flip Map.lookup) (snd ns) n of Nothing -> 0
                                                   Just x  -> length x
outdeg ns n = case (flip Map.lookup) (fst ns) n of Nothing -> 0
                                                   Just x  -> length x

precs :: (Eq b) => [(a, b)] -> b -> [a]
precs ns n = map fst $ filter ((==n).snd) ns

-- testing:
otest1 :: [(String, String)]
otest1 = [("abc","bcd"), ("bcd","cde"), ("cde", "deg"), ("cde","def") ]
ofact1 :: [(String, String)]
ofact1 = [("abcde","deg"), ("abcde", "def")]

\end{code}
\end{document}
