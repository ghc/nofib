{-
   **************************************************************
   * Filename      : Deterministic.hs                           *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 78                                         *
   **************************************************************
-}

module FST.Deterministic ( determinize  -- Makes an automaton deterministic and usefulS.
                     ) where

import FST.Automaton

import Data.List (sort, nub)

{- *************************************
   * Types for subsets.                *
   *************************************
-}

newtype SubSet  = SubSet [State] -- a subset is an ordered set
                                 -- without duplication.
type    SubSets = [SubSet]
type    Done    = SubSets
type    UnDone  = SubSets
type    SubTransitions a = [(SubSet, [(a,SubSet)])]

instance Eq (SubSet) where
 (SubSet xs) == (SubSet ys) = xs == ys

sub :: [State] -> SubSet
sub sts = SubSet $ sort $ nub sts

containsFinal :: Automaton a -> SubSet -> Bool
containsFinal automaton (SubSet xs) = or $ map (isFinal automaton) xs

{- ************************************************
   * Construct a deterministic, usefulS automaton. *
   ************************************************
-}

determinize :: Ord a => Automaton a -> Automaton a
determinize automaton = let inS = sub $ initials automaton in
                            det automaton ([],[inS]) []

det :: Ord a => Automaton a -> (Done,UnDone) ->
                SubTransitions a -> Automaton a
det auto (done,[]) trans = rename (reverse trans)
                                  (alphabet auto) [sub (initials auto)]
                                  (filter (containsFinal auto) done)
                                  (firstState auto)
det auto (done,subset:undone) trans
 | elemSS done subset = det auto (done,undone) trans
 | otherwise = let (subs,nTrans) = getTransitions auto subset trans
                   nsubs         = filter (not.(elemSS (subset:done))) subs
                in det auto (subset:done,undone++nsubs) nTrans
 where elemSS subs sub = elem sub subs

getTransitions :: Ord a => Automaton a -> SubSet ->
                           SubTransitions a -> (SubSets, SubTransitions a)
getTransitions auto subset@(SubSet xs) trans
   = let tr = groupBySymbols (concat $ map (transitionList auto) xs) [] in
         (map snd tr, ((subset,tr):trans))

groupBySymbols :: Eq a => [(a,State)] -> [(a,[State])] -> [(a,SubSet)]
groupBySymbols []         tr = map (\(a,xs) -> (a,sub xs)) tr
groupBySymbols ((a,s):xs) tr = groupBySymbols xs (ins (a,s) tr)
 where ins (a1,s1) [] = [(a1,[s1])]
       ins (a1,s1) ((b,ys):zs)
        | a1 == b    = (b,s1:ys):zs
        | otherwise = (b,ys): ins (a,s) zs
