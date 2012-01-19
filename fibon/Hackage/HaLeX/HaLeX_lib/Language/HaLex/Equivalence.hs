
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.RegExp
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005
-- License     :  LGPL
-- 
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Equivalence of Regular Expressions 
-- 
-- Code Included in the Lecture Notes on 
--          Language Processing (with a functional flavour).   
--
-----------------------------------------------------------------------------


module Language.HaLex.Equivalence ( 
                     equivDfa
                   , equivNdfa 
                   , equivRE
                   , equivREs
                   ) where

import Language.HaLex.RegExp
import Language.HaLex.RegExp2Fa
import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.FaOperations
import Language.HaLex.Minimize



-- | Test whether two 'Dfa' are quivalent or not.

equivDfa :: (Ord st, Ord sy) 
         => Dfa st sy         -- ^ Deterministic Automaton
         -> Dfa st sy         -- ^ Deterministic Automaton
         -> Bool              -- ^ Equivalent?

equivDfa dfa1 dfa2 = tdfa_1 == tdfa_2
 where canonicalDfa1 = beautifyDfa $ minimizeDfa dfa1
       canonicalDfa2 = beautifyDfa $ minimizeDfa dfa2
       tdfa_1        = dfa2tdfa canonicalDfa1
       tdfa_2        = dfa2tdfa canonicalDfa2


-- | Test whether two 'Ndfa' are quivalent or not.

equivNdfa :: (Ord st, Ord sy) 
          => Ndfa st sy        -- ^ Non-Deterministic Automaton
          -> Ndfa st sy        -- ^ Non-Deterministic Automaton
          -> Bool              -- ^ Equivalent?

equivNdfa ndfa1 ndfa2 = equivDfa (ndfa2dfa ndfa1) (ndfa2dfa ndfa2)


-- | Test whether two 'RegExp' are quivalent or not.

equivRE :: Ord sy 
        => RegExp sy           -- ^ Regular Expression
        -> RegExp sy           -- ^ Regular Expression
        -> Bool                -- ^ Equivalent?

equivRE re1 re2 = equivNdfa (regExp2Ndfa re1) (regExp2Ndfa re2)


-- | Test whether a list of 'RegExp' are quivalent or not.

equivREs :: Ord sy 
         => [RegExp sy]        -- ^ List of Regular Expressions
         -> Bool               -- ^ Equivalent?

equivREs re = or (map (equivRE reFst) (tail re))
  where reFst = head re












