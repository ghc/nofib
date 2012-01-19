{-
   **************************************************************
   * Filename      : Transducer.hs                              *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 144                                        *
   **************************************************************
-}

module FST.Transducer ( module FST.TransducerTypes,
                    Transducer, -- data type for a transducer
                    construct,  -- construct a transducer.
                    TConvertable, -- type class for conversion to
                                  -- an from a 'Transducer'.
                    decode, -- from a transducer to an structure.
                    encode, -- from a structure to a transducer.
                    rename,
                    initial,
                    transitions,
                    nullFirstState,
                    productT,
                    unionT,
                    starT,
                    compositionT,
                    showTransducer
                  ) where

import FST.TransducerTypes
import FST.Utils (tagging,remove,merge)

import Data.Maybe (fromJust)
import Data.List ((\\),nub,delete)

{- **********************************************************
   * data types for a transducer                            *
   **********************************************************
-}

data Transducer a = Transducer {
                                stateTrans  :: TTransitionTable a,
                                initS       :: InitialStates,
                                finalStates :: FinalStates,
                                alpha       :: Sigma a,
                                firstS      :: FirstState,
                                lastS       :: LastState
                               }
 deriving (Show,Read)

{- **********************************************************
   * Instance of TransducerFunctions                        *
   **********************************************************
-}

instance TransducerFunctions Transducer where
 states                 = (map fst).stateTrans
 isFinal a s            = elem s (finalStates a)
 initials               = initS
 finals                 = finalStates
 transitionTable        = stateTrans
 transitionList a s     = case (lookup s (stateTrans a)) of
                           Just xs -> xs
                           _       -> []
 transitionsU auto (s,a) = map (\((_,c),s1) -> (c,s1)) $
                        filter (\((b,_),_) -> a == b) (transitionList auto s)
 transitionsD auto (s,a) = map (\((c,_),s1) -> (c,s1)) $
                        filter (\((_,b),_) -> a == b) (transitionList auto s)
 lastState              = lastS
 firstState             = firstS
 alphabet               = alpha

initial :: Transducer a -> State
initial = head.initials

nullFirstState :: Transducer a -> Transducer a
nullFirstState transducer = transducer {firstS = 0}

transitions :: Eq a => Transducer a -> (State,Relation a) -> [State]
transitions transducer (s,r) = map snd $ filter (\(r1,_) -> r == r1)
                                          (transitionList transducer s)

{- **********************************************************
   * Construct a transducer                                 *
   **********************************************************
-}

construct :: (State,State) -> TTransitionTable a -> Sigma a ->
             InitialStates -> FinalStates -> Transducer a
construct bs table sigma is fs = Transducer {
                                            stateTrans  = table,
                                            initS       = is,
                                            finalStates = fs,
                                            firstS      = fst bs,
                                            lastS       = snd bs,
                                            alpha       = sigma
                                            }

{- **********************************************************
   * Type class TConvertable                                *
   **********************************************************
-}

class TConvertable f where
 encode :: Eq a => f a -> Transducer a
 decode :: Eq a => Transducer a -> f a

{- **********************************************************
   * Convert automaton labelled with something other than   *
   * states to an 'Automaton'.                              *
   **********************************************************
-}

rename :: Eq b => [(b,[(Relation a,b)])] -> Sigma a -> [b] -> [b] ->
                                          State -> Transducer a
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

{- ***********************************************************
   * Combine transducers                                     *
   ***********************************************************
-}

renameT :: Transducer a -> Transducer a -> (Transducer a,Transducer a,State)
renameT transducer1 transducer2 = let tr2 = rename
                                            (transitionTable transducer2)
                                            (alphabet transducer2)
                                            (initials transducer2)
                                            (finals transducer2)
                                            (lastState transducer1 +1)
                                    in (transducer1,tr2,lastState tr2 +1)

productT :: Eq a => Transducer a -> Transducer a -> Transducer a
productT transducer1 transducer2 = productT' $ renameT transducer1
                                                       transducer2
  where productT' (t1,t2,s) =
          let transUnion  = (remove (initial t1) (transitionTable t1)) ++
                            (remove (initial t2) (transitionTable t2))
              transConc   = let t = (transitionList t2 (initial t2)) in
                                     [(f,t)| f <- (finals t1)]
              transInit   = [(s, transitionList t1 (initial t1) ++
                            listEps t1 (transitionList t2 (initial t2)))]
              fs  = finals t2 ++ listEps t2 (finals t1) ++
                    if (acceptEpsilon t1 && acceptEpsilon t2)
                     then [s] else []
            in Transducer
                     {
                     stateTrans  = transInit ++ merge transConc transUnion,
                     finalStates = fs \\ [(initial t1),(initial t2)],
                     alpha  = nub $ alphabet t1 ++ alphabet t2,
                     initS  = [s],
                     firstS = firstState t1,
                     lastS   = s
                     }

unionT :: Eq a => Transducer a -> Transducer a -> Transducer a
unionT transducer1 transducer2 = unionT' $ renameT transducer1 transducer2
 where unionT' (t1,t2,s) =
        let transUnion  = (remove (initial t1) (transitionTable t1)) ++
                        (remove (initial t2) (transitionTable t2))
            transInit   = [(s, transitionList t1 (initial t1) ++
                             transitionList t2 (initial t2))]
            fs  = finals t1 ++ finals t2 ++
                if (acceptEpsilon t1 || acceptEpsilon t2)
                    then [s] else []
         in Transducer
                    {
                     stateTrans  = transInit ++ transUnion,
                     finalStates = fs \\ [(initial t1),(initial t2)],
                     alpha = nub $ alphabet t1 ++ alphabet t2,
                     initS  = [s],
                     firstS = firstState t1,
                     lastS   = s
                    }

starT :: Eq a => Transducer a -> Transducer a
starT t1
 = let s = lastState t1 +1
       transUnion  = remove (initial t1) (transitionTable t1)
       transLoop   = let t = transitionList t1 (initial t1) in
                         (s,t): [(f,t) | f <- finals t1]
    in Transducer  {
                     stateTrans  = merge transLoop transUnion,
                     finalStates = (s:(delete (initial t1) (finals t1))),
                     alpha       = alphabet t1,
                     initS       = [s],
                     firstS      = firstState t1,
                     lastS       = s
                    }

compositionT :: Eq a => Transducer a -> Transducer a -> Transducer a
compositionT t1 t2 =
      let minS1 = firstState t1
          minS2 = firstState t2
          name (s1,s2) = (lastState t2 - minS2 +1) *
                         (s1 - minS1) + s2 - minS2 + minS1
          nS = name (lastState t1,lastState t2) +1
          transInit = (nS,[((a,d),name (s1,s2)) |
                                         ((a,b),s1) <- ((Eps,Eps),initial t1):transitionList
                                                    t1 (initial t1),
                                         ((c,d),s2) <- ((Eps,Eps),initial t2):transitionList
                                                    t2 (initial t2),
                                         ((a,b) /= (Eps,Eps)) || ((c,d) /= (Eps,Eps)),
                                         b == c])
          transTable = [(name (s1,s2),[((a,d),name (s3,s4)) |  ((a,b),s3)   <- ((Eps,Eps),s1):tl1,
                                                               ((c,d),s4)   <- ((Eps,Eps),s2):tl2,
                                                               ((a,b) /= (Eps,Eps)) || ((c,d) /= (Eps,Eps)),
                                                               b == c]) |
                                              (s1,tl1) <- transitionTable t1,
                                              (s2,tl2) <- transitionTable t2,
                                              s1 /= initial t1 ||
                                              s2 /= initial t2
                                               ]
          transUnion = transInit:transTable
          fs  = (if (acceptEpsilon t1 && acceptEpsilon t2)
                 then [nS] else []) ++
                   [name (f1,f2)| f1 <- finals t1,
                                  f2 <- finals t2]
       in Transducer
                           {
                            stateTrans  = merge [(s,[]) | s <- fs] transUnion,
                            finalStates = fs,
                            alpha  = nub $ alphabet t1 ++ alphabet t2 ,
                            initS  = [nS],
                            firstS = min (firstState t1) (firstState t2),
                            lastS  = nS
                            }

acceptEpsilon :: Transducer a -> Bool
acceptEpsilon transducer = isFinal transducer (initial transducer)

listEps :: Transducer a -> [b] -> [b]
listEps transducer xs
 | acceptEpsilon transducer = xs
 | otherwise                = []

{- ***********************************************************
   * Display a transducer                                    *
   ***********************************************************
-}

showTransducer :: Show a => Transducer a -> String
showTransducer transducer
  = "\n>>>> Transducer Construction <<<<" ++
    "\n\nTransitions:\n"       ++ aux  (stateTrans transducer)     ++
    "\nNumber of States      => " ++ show (length (transitionTable transducer)) ++
    "\nNumber of Transitions => " ++ show (sum [length tl | (s,tl) <- transitionTable transducer]) ++
    "\nAlphabet              => " ++ show (alphabet transducer)       ++
    "\nInitials              => " ++ show (initials transducer)       ++
    "\nFinals                => " ++ show (finals transducer)         ++ "\n"
  where aux []          = []
        aux ((s,tl):xs) = show s ++" => " ++ aux2 tl ++ "\n" ++ aux xs
        aux2 [] = []
        aux2 ((r,s):tl)  = "( " ++  showR r ++ " ," ++ show s ++") " ++ aux2 tl
        showR (S a, S b) = "(" ++ show a ++":" ++ show b ++ ")"
        showR (S a, Eps) = "(" ++ show a ++":eps)"
        showR (Eps, S b) = "(eps:" ++ show b ++ ")"
        showR (Eps, Eps) = "(eps:eps)"
