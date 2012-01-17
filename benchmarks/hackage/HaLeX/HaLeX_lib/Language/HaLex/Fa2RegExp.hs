-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.Fa2RegExp
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005
-- License     :  LGPL
--
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- From Finite Automata into Regular Expressions
--
-- Code Included in the Lecture Notes on
--        Language Processing (with a functional flavour).
--
-----------------------------------------------------------------------------

module Language.HaLex.Fa2RegExp ( dfa2RegExp
                                , regExpFromTo
                                , ndfaregExpFromTo
                                ) where


import Data.List
import Language.HaLex.Util
import Language.HaLex.RegExp
import Language.HaLex.Dfa
import Language.HaLex.Ndfa


-- | Compute a regular expression that defines the transitions from an
--   origin to a destination in a 'Dfa'.

regExpFromTo :: Eq st
             => (st -> sy -> st)       -- ^ Transition Function
             -> [sy]                   -- ^ Vocabulary
             -> st                     -- ^ Origin State
             -> st                     -- ^ Destination State
             -> RegExp sy              -- ^ Regular Expression

regExpFromTo delta v o d = let sys = transitionsFromTo delta v o d
                           in  toRegExp sys

toRegExp :: [sy] -> RegExp sy
toRegExp []     = Empty
toRegExp (x:xs) = toRegExp2 (x:xs)

toRegExp2 []     = Epsilon
toRegExp2 [x]    = Literal x
toRegExp2 (x:xs) = Or (Literal x) (toRegExp2 xs)


-- | Compute a regular expression that defines the transitions from an
--   origin to a destination in a 'Ndfa'.

ndfaregExpFromTo :: Eq st
                 => (st -> Maybe sy -> [st])   -- ^ Transition Function
                 -> [sy]                       -- ^ Vocabulary
                 -> st                         -- ^ Origin State
                 -> st                         -- ^ Destination State
                 -> RegExp sy                  -- ^ Regular Expression

ndfaregExpFromTo delta v o d = let sys = ndfaTransitionsFromTo delta v o d
                               in  toRegExp' sys

toRegExp' :: [Maybe sy] -> RegExp sy
toRegExp' []     = Empty
toRegExp' (x:xs) = toRegExp2' (x:xs)

toRegExp2' []     = Epsilon
toRegExp2' [x]    = case x of
                      Nothing    -> Epsilon
                      (Just x )  -> Literal x
toRegExp2' (x:xs) = case x of
                      Nothing    -> Or Epsilon (toRegExp2' xs)
                      (Just x )  -> Or (Literal x) (toRegExp2' xs)



regular :: Num st
        => (st -> sy -> st)
        -> [sy]
        -> st
        -> st
        -> st
        -> RegExp sy

regular d v i j 0
      |  i==j      =  (regExpFromTo d v i j) `Or` Epsilon
      | otherwise  =  regExpFromTo d v i j              -- force to Int !!
regular d v i j k  =  Or (Then (Then (regular d v i k (k-1))
                                  (Star (regular d v k k (k-1))  )  )
                         (regular d v k j (k-1))) (regular d v i j (k-1))


-- | Compute a regular expression from a 'Dfa'.

dfa2RegExp :: Eq sy
           => Dfa Int sy            -- ^ Deterministic Automaton
           -> RegExp sy             -- ^ Equivalent Regular Expression
dfa2RegExp dfa@(Dfa v q s z delta) =
          limit simplifyRegExp (applyD delta v s z (sizeDfa dfa))

applyD :: Num st
       => (st -> sy -> st)
       -> [sy]
       -> st
       -> [st]
       -> st
       -> RegExp sy

applyD d v _ []     _   = Epsilon
applyD d v i (z:zs) k   = (regular d v i z k) `Or` (applyD d v i zs k)


