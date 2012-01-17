{-
   **************************************************************
   * Filename      : MinimalTBrzozowski.hs                      *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 7 July, 2001                               *
   * Lines         : 20                                         *
   **************************************************************
-}

module FST.MinimalTBrzozowski ( minimize -- minimize an automaton.
                          ) where

import FST.Transducer
import FST.DeterministicT
import FST.ReversalT

{- An algorithm due to Brzozowski -}

{-# SPECIALIZE minimize :: Transducer String -> Transducer String #-}

minimize :: Ord a => Transducer a -> Transducer a
minimize = determinize.reversal.determinize.reversal
