{-
   **************************************************************
   * Filename      : AutomatonTypes.hs                          *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 71                                         *
   **************************************************************
-}

module FST.AutomatonTypes ( State,          -- State.
                        FirstState,     -- the first state.
                        LastState,      -- the last state.
                        InitialStates,  -- the initial states.
                        FinalStates,    -- set of final states.
                        Transitions,    -- set of transitions.
                        TransitionTable, -- table of transitions.
                        Sigma,           -- the alphabet of an automaton.
                        AutomatonFunctions, -- Type class of automaton
                                            -- functions.
                        states,  -- get the states of an automaton.
                        isFinal, -- is the given state a final state?
                        initials, -- get the initial states of an automaton.
                        finals,  -- get the final states of an automaton.
                        transitionTable, -- get the transitionTable.
                        transitionList, -- get the transitions w.r.t. a state.
                        transitions, -- get the transitions
                                     -- w.r.t. a state and a symbol.
                        firstState,
                        lastState, -- get the maximum state of a automaton.
                        alphabet -- get the alphabet of an automaton.
                       ) where

-- Types for Automaton

type State = Int

type FirstState = Int

type LastState = Int

type InitialStates = [State]

type FinalStates = [State]

type Transitions a = [(a,State)]

type TransitionTable a = [(State,Transitions a)]

type Sigma a = [a]

-- | Class of AutomatonFunctions
class AutomatonFunctions f where
 states          :: f a -> [State]
 isFinal         :: f a -> State -> Bool
 finals          :: f a -> FinalStates
 initials        :: f a -> InitialStates
 transitionList  :: f a -> State -> Transitions a
 transitionTable :: f a -> TransitionTable a
 transitions     :: Eq a => f a -> (State, a) -> [State]
 firstState      :: Eq a => f a -> State
 lastState       :: Eq a => f a -> State
 alphabet        :: f a -> Sigma a
