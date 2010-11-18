{-
   **************************************************************
   * Filename      : LBFT.hs                                    *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 7 July, 2001                               *
   * Lines         : 277                                        *
   **************************************************************
-}

module FST.LBFT ( LBFT (..),
              module FST.Transducer,
              compileToLBFT,
              compileToTransducer
             ) where

import Data.List (delete,nub,(\\))
import FST.EpsilonFreeT
import FST.RRegTypes
import FST.StateMonad
import FST.Transducer
import FST.Utils (merge,remove)
import qualified FST.AutomatonInterface as A

{- **********************************************************
   * data type for a LBFT                                   *
   **********************************************************
-}

data LBFT a = LBFT {
                     trans   :: TTransitionTable a,
                     initS   :: State,
                     finalS  :: [State],
                     alpha   :: Sigma a,
                     lastS   :: State
                   }

{- **********************************************************
   * LBFT functions                                         *
   **********************************************************
-}

instance TransducerFunctions LBFT where
 states                  = (map fst).trans
 isFinal t s             = elem s (finals t)
 initials t              = [initS t]
 finals                  = finalS
 transitionTable         = trans
 transitionList t s      = case (lookup s (trans t)) of
                            Just xs -> xs
                            _       -> []
 transitionsU t (s,a)    = map (\((_,c),s1) -> (c,s1)) $
                           filter (\((b,_),_) -> a == b) $ transitionList t s
 transitionsD t (s,a)    = map (\((c,_),s1) -> (c,s1)) $
                           filter (\((_,b),_) -> a == b) $ transitionList t s
 lastState               = lastS
 firstState              = minimum.states
 alphabet                = alpha

acceptEpsilon :: LBFT a -> Bool
acceptEpsilon lbft = isFinal lbft (initialLBFT lbft)

initialLBFT :: LBFT a -> State
initialLBFT = initS

{- **********************************************************
   * compile a regular relation to a LBFT                   *
   **********************************************************
-}

compileToLBFT :: Ord a => RReg a -> Sigma a -> LBFT a
compileToLBFT reg sigma = run (build reg (nub (sigma++symbols reg))) 0

{- ************************************************************************
   * compile a regular relation to an minimal, useful and deterministic   *
   * transducer, using the LBFT algorithm while building.                 *
   ************************************************************************
-}
compileToTransducer :: Ord a => RReg a -> Sigma a -> Transducer a
compileToTransducer reg sigma = encode $ compileToLBFT reg sigma

{- ************************************************************************
   * Building a LBFT from a regular relation                              *
   ************************************************************************
-}

build :: Ord a => RReg a -> Sigma a -> STM (LBFT a)
build (EmptyR) sigma = do s <- fetchState
                          return $ LBFT {
                                         trans  = [(s,[])],
                                         initS  = s,
                                         finalS = [],
                                         alpha  = sigma,
                                         lastS   = s
                                       }

build (Relation a b) sigma
  = do s1 <- fetchState
       s2 <- fetchState
       return $ LBFT {
                      trans  = [(s1,[((a,b),s2)]),(s2,[])],
                      initS  = s1,
                      finalS = [s2],
                      alpha  = sigma,
                      lastS   = s2
                     }

build (Identity r1) sigma
  = do s <- fetchState
       let auto = A.compileNFA r1 sigma s
           nTrans = [(s1,map (\(a,s2) -> ((S a, S a),s2))
                         (A.transitionList auto s1)) | s1 <- A.states auto]
       setState (A.lastState auto+1)
       return $ LBFT {
                      trans  = nTrans,
                      initS  = head (A.initials auto),
                      finalS = A.finals auto,
                      alpha  = sigma,
                      lastS  = A.lastState auto
                     }

build (ProductR r1 r2) sigma
 = do lbft1 <- build r1 sigma
      lbft2 <- build r2 sigma
      s <- fetchState
      let transUnion  = (remove (initialLBFT lbft1) (trans lbft1)) ++
                        (remove (initialLBFT lbft2) (trans lbft2))
          transConc   = let t = (transitionList lbft2 (initialLBFT lbft2)) in
                                [(f,t)| f <- (finals lbft1)]
          transInit   = [(s, transitionList lbft1 (initialLBFT lbft1) ++
                        listEps lbft1 (transitionList lbft2 (initialLBFT lbft2)))]
          fs  = finals lbft2 ++ listEps lbft2 (finals lbft1) ++
                if (acceptEpsilon lbft1 && acceptEpsilon lbft2)
                   then [s] else []
      return $ LBFT {
                     trans  = transInit ++ merge transConc transUnion,
                     finalS = fs \\ [(initialLBFT lbft1),(initialLBFT lbft2)],
                     alpha  = sigma,
                     initS  = s,
                     lastS   = s
                     }

build (UnionR r1 r2) sigma
 = do lbft1 <- build r1 sigma
      lbft2 <- build r2 sigma
      s <- fetchState
      let transUnion  = (remove (initialLBFT lbft1) (trans lbft1)) ++
                        (remove (initialLBFT lbft2) (trans lbft2))
          transInit   = [(s, transitionList lbft1 (initialLBFT lbft1) ++
                             transitionList lbft2 (initialLBFT lbft2))]
          fs  = finals lbft1 ++ finals lbft2 ++
                if (acceptEpsilon lbft1 || acceptEpsilon lbft2)
                    then [s] else []
      return $ LBFT {
                     trans  = transInit ++ transUnion,
                     finalS = fs \\ [(initialLBFT lbft1),(initialLBFT lbft2)],
                     alpha = sigma,
                     initS  = s,
                     lastS   = s
                    }

build (StarR r1) sigma
 = do lbft1 <- build r1 sigma
      s <- fetchState
      let transUnion  = remove (initialLBFT lbft1) (trans lbft1)
          transLoop   = let t = transitionList lbft1 (initialLBFT lbft1) in
                         (s,t): [(f,t) | f <- finals lbft1]
      return $ LBFT {
                     trans   = merge transLoop transUnion,
                     finalS  = (s:(delete (initialLBFT lbft1) (finals lbft1))),
                     alpha   = sigma,
                     initS   = s,
                     lastS   = s
                    }

build (Cross r1 r2) sigma =
 do s <- fetchState
    let auto1 = A.compileNFA r1 sigma s
        auto2 = A.compileNFA r2 sigma s
        (trTable,fs) = cross auto1 auto2
                     ([],[(A.initial auto1,A.initial auto2)]) ([],[])
        lbft = decode $ rename trTable sigma
                         [(A.initial auto1,A.initial auto2)] fs s
    setState $ lastState lbft + 1
    return lbft
  where cross _ _ (_,[]) result = result
        cross auto1 auto2 (done,((s1,s2):undone)) (tr,fs) =
         let tl = combine auto1 auto2 (A.transitionList auto1 s1)
                                      (A.transitionList auto2 s2) (s1,s2)
             nSts = (map snd tl) \\ ((s1,s2):done)
          in cross auto1 auto2 ((s1,s2):done,nSts++undone) (((s1,s2),tl):tr,
               if (A.isFinal auto1 s1 && A.isFinal auto2 s2) then
                                ((s1,s2):fs) else fs)
        combine _ _ [] [] _       = []
        combine _ _ xs [] (_,s2)  = [((S a,Eps),(s1,s2)) | (a,s1) <- xs]
        combine _ _ [] ys (s1,_)  = [((Eps,S b),(s1,s2)) | (b,s2) <- ys]
        combine auto1 auto2 xs ys (s1,s2)
         = [((S a, S b), (s3,s4)) | (a,s3) <- xs, (b,s4) <- ys] ++
           (if (A.isFinal auto1 s1) then [((Eps,S b),(s1,s4)) | (b,s4) <- ys]
             else []) ++
           (if (A.isFinal auto2 s2) then [((S a,Eps),(s3,s2)) | (a,s3) <- xs]
             else [])

build (Comp r1 r2) sigma
 = do lbft1 <- build r1 sigma
      lbft2 <- build r2 sigma
      let minS1 = firstState lbft1
          minS2 = firstState lbft2
          name (s1,s2) = (lastState lbft2 - minS2 +1) *
                         (s1 - minS1) + s2 - minS2 + minS1
          nS = name (lastState lbft1,lastState lbft2) +1
          transInit = (nS,[((a,d),name (s1,s2)) |
                                         ((a,b),s1) <- ((Eps,Eps),initialLBFT lbft1):transitionList
                                                    lbft1 (initialLBFT lbft1),
                                         ((c,d),s2) <- ((Eps,Eps),initialLBFT lbft2):transitionList
                                                    lbft2 (initialLBFT lbft2),
                                         ((a,b) /= (Eps,Eps)) || ((c,d) /= (Eps,Eps)),
                                         b == c])
          transTable = [(name (s1,s2),[((a,d),name (s3,s4)) | ((a,b),s3)   <- ((Eps,Eps),s1):tl1,
                                                              ((c,d),s4)   <- ((Eps,Eps),s2):tl2,
                                                              ((a,b) /= (Eps,Eps)) || ((c,d) /= (Eps,Eps)),
                                                               b == c]) |
                                              (s1,tl1) <- trans lbft1,
                                              (s2,tl2) <- trans lbft2,
                                              s1 /= initialLBFT lbft1 ||
                                              s2 /= initialLBFT lbft2]
          transUnion = transInit : transTable
          fs  = (if (acceptEpsilon lbft1 && acceptEpsilon lbft2)
                 then [nS] else []) ++
                 [name (f1,f2)| f1 <- finals lbft1, f2 <- finals lbft2]
      setState $ nS +1
      return $ decode $ epsilonfree $ encode $
               LBFT {trans  = merge [(s,[]) | s <- fs] transUnion
                              ,finalS = fs,alpha  = sigma,
                     initS  = nS,lastS   = nS}

{- **********************************************************
   * Instance of Convertable (LBFT a)                       *
   **********************************************************
-}

instance TConvertable LBFT where
 encode lbft = rename (trans lbft) (alphabet lbft) (initials lbft)
                      (finals lbft) (firstState lbft)
 decode t     = LBFT {
                     trans  = transitionTable t,
                     initS  = head (initials t),
                     finalS = finals t,
                     alpha  = alphabet t,
                     lastS  = lastState t
                    }

{- **********************************************************
   * Instance of Show (LBFT a)                              *
   **********************************************************
-}

instance (Eq a,Show a) => Show (LBFT a) where
 show t = "\n>>>> LBFT Construction <<<<" ++
             "\n\nTransitions:\n"         ++ aux  (trans t)          ++
             "\nNumber of States   => "   ++ show (length (trans t))   ++
             "\nInitial            => "   ++ show (initialLBFT t)        ++
             "\nFinals             => "   ++ show (finals t)  ++ "\n"
        where aux []          = []
              aux ((s,tl):xs) = show s ++" => " ++ show tl ++ "\n" ++ aux xs


{- **********************************************************
   * Auxiliary functions                                    *
   **********************************************************
-}

listEps :: LBFT a -> [b] -> [b]
listEps lbft xs
 | acceptEpsilon lbft = xs
 | otherwise          = []
