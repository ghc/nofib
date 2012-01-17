{-
   **************************************************************
   * Filename      : Reversal.hs                                *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 7 July, 2001                               *
   * Lines         : 28                                         *
   **************************************************************
-}

module FST.Reversal ( reversal  -- Reverse an automaton.
                ) where

import FST.Automaton

import Data.Array

reversal :: Eq a => Automaton a -> Automaton a
reversal automaton  = reverseTrans (rename (transitionTable automaton)
                                           (alphabet automaton)
                                           (finals automaton)
                                           (initials automaton)
                                           (firstState automaton))

reverseTrans :: Eq a => Automaton a -> Automaton a
reverseTrans automaton = let bs    = (firstState automaton, lastState automaton)
                             table = assocs $ accumArray (\tl1 tl2 -> tl1 ++ tl2) []
                                      bs [(s1,[(a,s)]) | (s,tl) <- transitionTable automaton,
                                                         (a,s1) <-  tl]
                          in construct bs table (alphabet automaton) (initials automaton) (finals automaton)
