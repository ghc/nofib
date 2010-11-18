{-
   **************************************************************
   * Filename      : EpsilonFreeT.hs                            *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 7 July, 2001                               *
   * Lines         : 46                                         *
   **************************************************************
-}

module FST.EpsilonFreeT (epsilonfree -- construct an epsilonfree,
                                 -- usefulS transducer.
                    ) where

import FST.Transducer
import Data.List (partition)

epsilonfree :: Eq a => Transducer a -> Transducer a
epsilonfree transducer
 = epsFree transducer ([],initials transducer) [] []

epsFree :: Eq a => Transducer a -> ([State],[State]) -> FinalStates ->
                   [(State,[(Relation a,State)])] -> Transducer a
epsFree transducer (_,[]) fs table
 = construct (firstState transducer, lastState transducer)
             table (alphabet transducer) (initials transducer) fs
epsFree transducer (done,(s:undone)) fs table
 = let (newtl,fsB) = stateEpsRemove [] (transitionList transducer s)
                                       ([],False)
       newSts    = map snd $ filter (\(_,s1) -> not (elem s1 (s:done))) newtl
    in epsFree transducer (s:done,newSts ++ undone)
       (if (fsB || isFinal transducer s) then (s:fs) else fs)
       ((s,newtl):table)
 where epsTransitions = ( \ ((a,b),_) -> (a == Eps) && (b == Eps) )
       stateEpsRemove _ [] (tl,fsB) = (tl,fsB)
       stateEpsRemove history tlist (tl,fsB)
        = case (partition epsTransitions tlist) of
            (  [],ntl) -> (tl++ntl,fsB)
            (epstl,ntl) -> let newSts  = map snd $
                                 filter (\(_,s1) -> not (elem s1 history))
                                                    epstl
                               fsBnew  = or $ map (isFinal transducer) newSts
                             in stateEpsRemove (newSts++history)
                                  (concat (map (transitionList transducer)
                                                newSts))
                                           (ntl++tl,fsB || fsBnew)
