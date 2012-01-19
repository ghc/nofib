{-
   **************************************************************
   * Filename      : TransducerTypes.hs                         *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 75                                         *
   **************************************************************
-}

module FST.TransducerTypes ( State,
                         FinalStates,
                         FirstState,
                         LastState,
                         Sigma,
                         Relation,
                         Upper,
                         Lower,
                         Symbol (..),
                         TTransitions,
                         TTransitionTable,
                         InitialStates,
                         TransducerFunctions,
                         states,
                         isFinal,
                         initials,
                         finals,
                         transitionTable,
                         transitionList,
                         transitionsU,
                         transitionsD,
                         firstState,
                         lastState,
                         alphabet
                       ) where

import FST.AutomatonTypes (State,FinalStates,Sigma,FirstState,LastState,
                       InitialStates)

{- **********************************************************
   * Transducer types                                       *
   **********************************************************
-}

type Relation a = (Upper a, Lower a)
type Upper a = Symbol a
type Lower a = Symbol a

data Symbol a
 =    S a   |
      Eps
    deriving (Show,Read,Eq)

type TTransitions a = [(Relation a,State)]

type TTransitionTable a = [(State,[(Relation a,State)])]

{- **********************************************************
   * Class of TransducerFunctions                           *
   **********************************************************
-}

class TransducerFunctions f where
 states         :: f a -> [State]
 isFinal        :: f a -> State -> Bool
 initials       :: f a -> InitialStates
 finals         :: f a -> FinalStates
 transitionTable :: f a -> TTransitionTable a
 transitionList :: f a -> State -> TTransitions a
 transitionsU    :: Eq a => f a -> (State, Symbol a) -> [(Symbol a, State)]
 transitionsD    :: Eq a => f a -> (State, Symbol a) -> [(Symbol a, State)]
 firstState     :: f a -> State
 lastState      :: f a -> State
 alphabet       :: f a -> Sigma a
