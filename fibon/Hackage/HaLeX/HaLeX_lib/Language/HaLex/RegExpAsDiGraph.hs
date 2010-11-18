-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.RegExpAsDiGraph
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005
-- License     :  LGPL
-- 
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Regular Expressions as Directed Graphs (in GraphViz)
--
-- Code Included in the Lecture Notes on 
--      Language Processing (with a functional flavour).   
--
-----------------------------------------------------------------------------

module Language.HaLex.RegExpAsDiGraph ( 
                         re2graphviz
                       ) where

import Language.HaLex.RegExp
import Language.HaLex.RegExp2Fa
--import Language.HaLex.RegExpParser
import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.FaOperations
import Language.HaLex.Minimize
import Language.HaLex.FaAsDiGraph

-- | Print a 'RegExp' in GraphViz-dot (as a string) 
re2graphviz :: (Ord sy, Show sy)
            => RegExp sy  -- ^ Regular Expression (Abstract)
            -> [Char]     -- ^ Graph's Name
            -> Bool       -- ^ True: Deterministic ; False: Non-Deterministic
            -> Bool       -- ^ Minimized?
            -> Bool       -- ^ Beautified?   (states as numbers)
            -> Bool       -- ^ Dead States?
            -> [Char]     -- ^ dot sentence

re2graphviz re n d m b s 
  | not d                        = re2DiGraphNdfa re n
  | d && not m          && s     = re2DiGraph    re n
  | d && not m && b     && not s = re2DiGraph'   re n
  | d && m     && b     && s     = re2DiGraph''' re n
  | d && m     && b     && not s = re2DiGraph''  re n
  | otherwise                    = re2DiGraph''' re n



re2DiGraph :: (Show sy, Ord sy) 
           => RegExp sy               -- ^ Regular Expression
           -> [Char]                  -- ^ Module Name
           -> [Char]                  -- ^ Dot Graph
re2DiGraph re name = dfa2graphviz dfa name 
  where dfa = (ndfa2dfa . regExp2Ndfa) re

re2DiGraph' re name = dfa2graphviz dfa name 
  where dfa = (beautifyDfa .ndfa2dfa . regExp2Ndfa) re

re2DiGraph'' re name = dfa2graphviz dfa name 
  where dfa = (beautifyDfa . minimizeDfa . ndfa2dfa . regExp2Ndfa) re


re2DiGraph''' re name = dfa2DiGraphWithNoSyncSt dfa name 
  where dfa = (beautifyDfaWithSyncSt . minimizeDfa . ndfa2dfa . regExp2Ndfa) re

re2DiGraphNdfa re name = ndfa2graphviz ndfa name
  where ndfa = regExp2Ndfa re  


re2DiGraphIO :: (Show sy, Ord sy) => RegExp sy -> [Char] -> IO ()
re2DiGraphIO er name = dfa2graphviz2file dfa name 
  where dfa = (beautifyDfa . ndfa2dfa . regExp2Ndfa) er


absRe2DiGraph_File er name = 
        dfaDiGraphWithNoSyncStIO dfa name (name++".dot") 
  where dfa = (beautifyDfaWithSyncSt . minimizeDfa . ndfa2dfa . regExp2Ndfa) er








