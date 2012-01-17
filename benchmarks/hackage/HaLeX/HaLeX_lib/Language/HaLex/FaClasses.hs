{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.Dfa
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005
-- License     :  LGPL
--
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- The Class of Finite Automaton in Haskell
--
-- Code Included in the Lecture Notes on
--      Language Processing (with a functional flavour).
--
-----------------------------------------------------------------------------

module Language.HaLex.FaClasses where

import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.FaOperations
import Language.HaLex.Equivalence
import Language.HaLex.Minimize
import Language.HaLex.FaAsDiGraph


-- | Class of Finite automaton

class Fa fa st sy where
  accept     :: fa st sy -> [sy] -> Bool
  sizeFa     :: fa st sy -> Int
  equiv      :: fa st sy -> fa st sy -> Bool
  minimize   :: fa st sy -> Dfa [[st]] sy
  reverseFa  :: fa st sy -> Ndfa st sy
  deadstates :: fa st sy -> [st]
  toHaskell' :: fa st sy -> String -> IO ()
  toGraph    :: fa st sy -> String -> String
  toGraphIO  :: fa st sy -> String -> IO()


  unionFa    :: fa st sy -> fa st sy -> Ndfa st sy
  concatFa   :: fa st sy -> fa st sy -> Ndfa st sy
  starFa     :: fa st sy -> Ndfa st sy
  plusFa     :: fa st sy -> Ndfa st sy


-- | Instance of class 'Fa' for a 'Dfa'

instance (Show st , Show sy , Ord st , Ord sy) => Fa Dfa st sy where
  accept     = dfaaccept
  sizeFa     = sizeDfa
  equiv      = equivDfa
  minimize   = minimizeDfa
  reverseFa  = reverseDfa
  deadstates = dfadeadstates
  toHaskell' = toHaskell
  toGraph    = dfa2graphviz
  toGraphIO  = dfa2graphviz2file

  unionFa    = unionDfa
  starFa     = starDfa
  concatFa   = concatDfa
  plusFa     = plusDfa

-- | Instance of class 'Fa' for a 'Ndfa'

instance (Show st , Show sy , Ord st , Ord sy) => Fa Ndfa st sy where
  accept     = ndfaaccept
  sizeFa     = sizeNdfa
  equiv      = equivNdfa
  minimize   = minimizeNdfa
  reverseFa  = reverseNdfa
  deadstates = ndfadeadstates
  toHaskell' = toHaskell
  toGraph    = ndfa2graphviz
  toGraphIO  = ndfa2graphviz2file

  unionFa    = unionNdfa
  starFa     = starNdfa
  concatFa   = concatNdfa
  plusFa     = plusNdfa


