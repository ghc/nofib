{-
   **************************************************************
   * Filename      : Automaton.hs                               *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 120                                        *
   **************************************************************
-}

module FST.Automaton ( module FST.AutomatonTypes,
                   Automaton, -- data type for an automaton
                   construct, -- construct an automaton.
                   Convertable, -- type class for conversion to
                                -- an from an 'Automaton'.
                   decode, -- from an automaton to an structure.
                   encode, -- from a structure to an Automaton.
                   rename,
                   showAutomaton
                  ) where

import FST.AutomatonTypes
import FST.Utils (tagging)

import Data.Maybe (fromJust)

-- data type for an automaton
data Automaton a = Automaton {
                              stateTrans     :: TransitionTable a,
                              initialStates  :: InitialStates,
                              finalStates    :: FinalStates,
                              alpha          :: Sigma a,
                              firstS         :: FirstState,
                              lastS          :: LastState
                             }
 deriving (Show,Read)

-- | Construct an automaton
construct :: (FirstState,LastState) -> TransitionTable a ->
             Sigma a -> InitialStates -> FinalStates -> Automaton a
construct bs table sigma inits fs = Automaton {
                                             stateTrans    = table,
                                             initialStates = inits,
                                             finalStates   = fs,
                                             alpha         = sigma,
                                             firstS        = fst bs,
                                             lastS         = snd bs
                                             }

-- |Instance of AutomatonFunctions
instance AutomatonFunctions Automaton where
 states                 = (map fst).stateTrans
 isFinal auto s         = elem s (finalStates auto)
 initials               = initialStates
 finals                 = finalStates
 transitionTable        = stateTrans
 transitionList auto s  = case (lookup s (stateTrans auto)) of
                           Just tl -> tl
                           _       -> []
 transitions auto (s,a) = map snd $ filter (\(b,_) -> b == a) $ transitionList auto s
 firstState             = firstS
 lastState              = lastS
 alphabet               = alpha

-- | Convert automaton labelled with something other than
--   states to an 'Automaton'.
rename :: Eq b => [(b,[(a,b)])] -> Sigma a -> [b] -> [b] ->
                                         State -> Automaton a
rename tTable sigma initS fs s
  = let (maxS,table) = tagging (map fst tTable) s
        nI           = map (\b  -> lookupState b table) initS
        nfs          = map (\b -> lookupState b table) fs
        nTrans       = renameTable tTable table
     in construct (s,maxS) nTrans sigma nI nfs
 where lookupState st tab = fromJust $ lookup st tab
       renameTable [] _ = []
       renameTable ((b,tl):tll) table
        = let s1  = lookupState b table
              ntl = map (\(a,b1) -> (a,lookupState b1 table)) tl
           in (s1,ntl):renameTable tll table

-- | Type class Convertable
class Convertable f where
 encode :: Eq a => f a -> Automaton a
 decode :: Eq a => Automaton a -> f a

-- | Display the automaton
showAutomaton :: Show a => Automaton a -> String
showAutomaton auto
  = "\n>>>> Automaton Construction <<<<" ++
    "\n\nTransitions:\n"       ++ aux  (stateTrans auto)     ++
    "\nNumber of States   => " ++ show (length (stateTrans auto)) ++
    "\nInitials           => " ++ show (initials auto)       ++
    "\nFinals             => " ++ show (finals auto)         ++ "\n"
  where aux []          = []
        aux ((s,tl):xs) = show s ++" => " ++ show tl ++ "\n" ++ aux xs
