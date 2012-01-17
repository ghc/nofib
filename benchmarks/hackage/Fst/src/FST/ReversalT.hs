{-
   **************************************************************
   * Filename      : ReversalT.hs                               *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 7 July, 2001                               *
   * Lines         : 30                                         *
   **************************************************************
-}

module FST.ReversalT ( reversal  -- Reverse a transducer.
                 ) where

import FST.Transducer

import Data.Array

reversal :: Eq a => Transducer a -> Transducer a
reversal transducer  = reverseTrans (rename (transitionTable transducer)
                                           (alphabet transducer)
                                           (finals transducer)
                                           (initials transducer)
                                           (firstState transducer))

reverseTrans :: Eq a => Transducer a -> Transducer a
reverseTrans transducer = let bs    = (firstState transducer, lastState transducer)
                              table = assocs $ accumArray (\tl1 tl2 -> tl1 ++ tl2) []
                                      bs [(s1,[(a,s)]) | (s,tl) <- transitionTable transducer,
                                                         (a,s1) <-  tl]
                          in construct bs table (alphabet transducer) (initials transducer) (finals transducer)
