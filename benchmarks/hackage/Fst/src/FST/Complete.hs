{-
   **************************************************************
   * Filename      : Complete.hs                                *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 29                                         *
   **************************************************************
-}

module FST.Complete ( complete -- Makes a automaton complete (transition on every symbol at every state)
                ) where

import FST.Automaton
import Data.List ( (\\) )

complete :: Eq a => Automaton a -> Automaton a
complete auto = let sink     = lastState auto + 1
                    sinkTr   = (sink,map (\a -> (a,sink)) (alphabet auto))
                    newTrans = sinkTr:completeStates auto sink (states auto) []
                 in construct (firstState auto,sink) newTrans (alphabet auto) (initials auto) (finals auto)

completeStates :: Eq a => Automaton a -> State -> [State] -> [(State,Transitions a)] -> [(State,Transitions a)]
completeStates _    _    []     trans = trans
completeStates auto sink (s:sts) trans
 = let tr   = transitionList auto s
       nTr  = map (\a -> (a,sink)) ((alphabet auto) \\ (map fst tr))
    in completeStates auto sink sts ((s,tr++nTr):trans)
