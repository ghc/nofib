{-
   **************************************************************
   * Filename      : LBFA.hs                                    *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 7 July, 2001                               *
   * Lines         : 279                                        *
   **************************************************************
-}

module FST.LBFA ( module FST.Automaton,
             LBFA,          -- Data type for LBFA
             states,        -- get the states of a LBFA
             finals,        -- get the final states of a LBFA
             isFinal,       -- check if a state is a final state.
             transitionTable,
             transitionList,   -- get the transitions of a state.
             transitions,    -- get the transitions of a state and a symbol
             alphabet,      -- get the alphabet of a LBFA.
             initial,       -- get the initial state of a LBFA.
             lastState,      -- get the max state of a LBFA.
             compileToLBFA,
             compileToAutomaton
            ) where

import FST.RegTypes
import FST.StateMonad
import FST.Automaton
import FST.Deterministic
import FST.Complete
import FST.Utils (remove,merge)

import Data.List (delete,nub,(\\))

{- **********************************************************
   * data type for a LBFA                                   *
   **********************************************************
-}

data LBFA a = LBFA {
                     trans   :: [(State, Transitions a)],
                     initS   :: State,
                     finalS  :: [State],
                     alpha   :: Sigma a,
                     lastS   :: State
                   }

{- **********************************************************
   * LBFA functions                                         *
   **********************************************************
-}

instance AutomatonFunctions LBFA where
 states lbfa            = map fst $ trans lbfa
 isFinal lbfa s         = elem s (finals lbfa)
 initials lbfa          = [(initS lbfa)]
 finals                 = finalS
 transitionTable        = trans
 transitionList lbfa s  = case(lookup s (trans lbfa)) of
                           Just tl -> tl
                           _       -> []
 transitions lbfa (s,a) = map snd $ filter (\(b,_) -> a == b) $ transitionList lbfa s
 firstState             = minimum.states
 lastState              = lastS
 alphabet               = alpha

initial :: LBFA a -> State
initial lbfa = (initS lbfa)

acceptEpsilon :: LBFA a -> Bool
acceptEpsilon lbfa = isFinal lbfa (initial lbfa)

{- **********************************************************
   * compile a regular expression to a LBFA                 *
   **********************************************************
-}

compileToLBFA :: Ord a => Reg a -> Sigma a -> State -> LBFA a
compileToLBFA reg sigma s = run (build reg (nub (sigma++symbols reg))) s

{- ************************************************************************
   * compile a regular expression to an minimal, useful and deterministic *
   * Automaton, using the LBFA algorithm while building.                  *
   ************************************************************************
-}
compileToAutomaton :: Ord a => Reg a -> Sigma a -> State -> Automaton a
compileToAutomaton reg sigma s = encode $ compileToLBFA reg sigma s

{- ************************************************************************
   * Building a LBFA from a regular expression                            *
   ************************************************************************
-}

build :: Ord a => Reg a -> Sigma a -> STM (LBFA a)
build (Empty) sigma = do s <- fetchState
                         return $ LBFA {
                                         trans  = [(s,[])],
                                         initS  = s,
                                         finalS = [],
                                         alpha  = sigma,
                                         lastS   = s
                                       }

build (Epsilon) sigma = do s <- fetchState
                           return $ LBFA {
                                          trans  = [(s,[])],
                                          initS  = s,
                                          finalS = [s],
                                          alpha  = sigma,
                                          lastS   = s
                                         }

build (Symbol a) sigma = do s1 <- fetchState
                            s2 <- fetchState
                            return $ LBFA {
                                          trans  = [(s1,[(a,s2)]),(s2,[])],
                                          initS  = s1,
                                          finalS = [s2],
                                          alpha  = sigma,
                                          lastS   = s2
                                          }

build (All) sigma = build (allToSymbols sigma) sigma

build (r1 :.: r2) sigma
 = do lbfa1 <- build r1 sigma
      lbfa2 <- build r2 sigma
      s <- fetchState
      let transUnion  = (remove (initial lbfa1) (trans lbfa1)) ++
                        (remove (initial lbfa2) (trans lbfa2))
          transConc   = let t = (transitionList lbfa2 (initial lbfa2)) in
                                [(f,t)| f <- (finals lbfa1)]
          transInit   = [(s, transitionList lbfa1 (initial lbfa1) ++
                        listEps lbfa1 (transitionList lbfa2 (initial lbfa2)))]
          fs  = finals lbfa2 ++ listEps lbfa2 (finals lbfa1) ++
                if (acceptEpsilon lbfa1 && acceptEpsilon lbfa2)
                   then [s] else []
      return $ LBFA {
                     trans  = transInit ++ merge transConc transUnion,
                     finalS = fs \\ [(initial lbfa1),(initial lbfa2)],
                     alpha  = sigma,
                     initS  = s,
                     lastS   = s
                     }

build (r1 :|: r2) sigma
 = do lbfa1 <- build r1 sigma
      lbfa2 <- build r2 sigma
      s <- fetchState
      let transUnion  = (remove (initial lbfa1) (trans lbfa1)) ++
                        (remove (initial lbfa2) (trans lbfa2))
          transInit   = [(s, transitionList lbfa1 (initial lbfa1) ++
                             transitionList lbfa2 (initial lbfa2))]
          fs  = finals lbfa1 ++ finals lbfa2 ++
                if (acceptEpsilon lbfa1 || acceptEpsilon lbfa2)
                    then [s] else []
      return $ LBFA {
                     trans  = transInit ++ transUnion,
                     finalS = fs \\ [(initial lbfa1),(initial lbfa2)],
                     alpha = sigma,
                     initS  = s,
                     lastS   = s
                    }

build (Star r1) sigma
 = do lbfa1 <- build r1 sigma
      s <- fetchState
      let transUnion  = remove (initial lbfa1) (trans lbfa1)
          transLoop   = let t = transitionList lbfa1 (initial lbfa1) in
                         (s,t): [(f,t) | f <- finals lbfa1]
      return $ LBFA {
                     trans  = merge transLoop transUnion,
                     finalS = (s:(delete (initial lbfa1) (finals lbfa1))),
                     alpha = sigma,
                     initS  = s,
                     lastS   = s
                    }

build (Complement r1) sigma
 = do lbfa <- build r1 sigma
      let lbfa1 = decode $ determinize $ complete $ encode lbfa
      setState $ lastState lbfa1 +1
      return $ LBFA {
                     trans = trans lbfa1,
                     finalS = (states lbfa1) \\ (finals lbfa1),
                     alpha = sigma,
                     initS  = initial lbfa1,
                     lastS   = lastState lbfa1
                    }

build (r1 :&: r2) sigma
 = do lbfa1 <- build r1 sigma
      lbfa2 <- build r2 sigma
      let minS1 = firstState lbfa1
          minS2 = firstState lbfa2
          name (s1,s2) = (lastState lbfa2 - minS2 +1) *
                         (s1 - minS1) + s2 - minS2 + minS1
          nS = name (lastState lbfa1,lastState lbfa2) +1
          transInit = (nS,[(a,name (s1,s2)) | (a,s1) <- transitionList
                                                     lbfa1 (initial lbfa1),
                                                 (b,s2) <- transitionList
                                                     lbfa2 (initial lbfa2),
                                                 a == b])
          transTable = [(name (s1,s2),[(a,name (s3,s4)) | (a,s3)   <- tl1,
                                                          (b,s4)   <- tl2,
                                                          a == b ]) |
                                                          (s1,tl1) <- trans lbfa1,
                                                          (s2,tl2) <- trans lbfa2,
                                                          s1 /= initial lbfa1 ||
                                                          s2 /= initial lbfa2
                                                          ]
          transUnion = transInit:transTable
          fs  = (if (acceptEpsilon lbfa1 && acceptEpsilon lbfa2)
                 then [nS] else []) ++
                [name (f1,f2)| f1 <- finals lbfa1,
                               f2 <- finals lbfa2]
      setState $ nS +1
      return LBFA {
                   trans  = merge [(s,[]) | s <- fs] transUnion,
                   finalS = fs,
                   alpha  = sigma,
                   initS  = nS,
                   lastS   = nS
                  }

{- **********************************************************
   * Instance of Convertable (LBFA a)                       *
   **********************************************************
-}

instance Convertable LBFA where
 encode lbfa = construct (firstState lbfa,lastState lbfa) (trans lbfa)
                         (alphabet lbfa) (initials lbfa) (finals lbfa)
 decode auto = LBFA {
                    trans  = transitionTable auto,
                    initS  = head (initials auto),
                    finalS = finals auto,
                    alpha  = alphabet auto,
                    lastS   = lastState auto
                    }

{- **********************************************************
   * Instance of Show (LBFA a)                              *
   **********************************************************
-}

instance (Eq a,Show a) => Show (LBFA a) where
 show auto = "\n>>>> LBFA Construction <<<<" ++
             "\n\nTransitions:\n"         ++ aux  (trans auto)          ++
             "\nNumber of States   => "   ++ show countStates   ++
             "\nInitial            => "   ++ show (initial auto)        ++
             "\nFinals             => "   ++ show (finals auto)  ++ "\n"
        where aux []          = []
              aux ((s,tl):xs) = show s ++" => " ++ show tl ++ "\n" ++ aux xs
              countStates = length $ nub $ map fst (trans auto) ++
                                           finals auto

{- **********************************************************
   * Auxiliary functions                                    *
   **********************************************************
-}

listEps :: LBFA a -> [b] -> [b]
listEps lbfa xs
 | acceptEpsilon lbfa = xs
 | otherwise          = []
