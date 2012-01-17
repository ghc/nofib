{-
   **************************************************************
   * Filename      : AutomatonInterface.hs                      *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 58                                         *
   **************************************************************
-}

module FST.AutomatonInterface ( compileNFA,
                            minimize,
                            complete,
                            determinize,
                            compile,
                            Automaton,
                            states,
                            isFinal,
                            initial,
                            finals,
                            transitionList,
                            transitions,
                            showAutomaton,
                            module FST.RegTypes,
                            module FST.AutomatonTypes,
                            numberOfStates,
                            numberOfTransitions
                          ) where

import FST.Automaton
import FST.AutomatonTypes
import qualified FST.MinimalBrzozowski as M
import FST.Complete
import qualified FST.Deterministic as D
import qualified FST.LBFA as L
import FST.RegTypes

compileNFA :: Ord a => Reg a -> Sigma a -> State -> Automaton a
compileNFA reg sigma s = L.compileToAutomaton reg sigma s

minimize :: Ord a => Automaton a -> Automaton a
minimize automaton = M.minimize automaton

determinize :: Ord a => Automaton a -> Automaton a
determinize automaton = D.determinize automaton

compile :: Ord a => Reg a -> Sigma a -> State -> Automaton a
compile reg sigma s = minimize $ L.compileToAutomaton reg sigma s

initial :: Automaton a -> State
initial automaton = head $ initials automaton

numberOfStates :: Ord a => Automaton a -> Int
numberOfStates auto = length $ states auto

numberOfTransitions :: Ord a => Automaton a -> Int
numberOfTransitions auto = sum [length (transitionList auto s) |
                                s <- states auto]
