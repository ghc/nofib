{-
   **************************************************************
   * Filename      : MinimalBrzozowski.hs                       *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 7 July, 2001                               *
   * Lines         : 20                                         *
   **************************************************************
-}

module FST.MinimalBrzozowski ( minimize -- minimize an automaton.
                         ) where

import FST.Automaton
import FST.Reversal
import FST.Deterministic

{- An algorithm due to Brzozowski.
   Note that the determinize function must construct a
   automaton with the usefulS property. -}
{-# SPECIALIZE minimize :: Automaton String -> Automaton String #-}
minimize :: Ord a => Automaton a -> Automaton a
minimize = determinize.reversal.determinize.reversal
