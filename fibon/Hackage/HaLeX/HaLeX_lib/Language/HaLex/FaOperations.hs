--
-- Functions manipulating Finite Automata (DFA and NDFA)
--
--
-- Code Included in the Lecture Notes on
--
--      Language Processing (with a functional flavour)
--
--
-- copyright João Saraiva
--           Department of Computer Science,
--           University of Minho,
--           Braga, Portugal
--           jas@di.uminho.pt
--           2001
--





module Language.HaLex.FaOperations (
                      ndfa2dfa
                    , dfa2ndfa
                    , ndfa2ct
                    , CT
                    , lookupCT
                    , stsDfa
                    , concatNdfa
                    , unionNdfa
                    , starNdfa
                    , plusNdfa
                    , expNdfa
                    , unionDfa
                    , concatDfa
                    , starDfa
                    , plusDfa
                    ) where

import Data.List
import Language.HaLex.Util
import Language.HaLex.Dfa
import Language.HaLex.Ndfa



--
-- Making a DFA from a NDFA
--

type StDfa st = [st]

type CT st = [( StDfa st, [StDfa st])]

stsDfa   = map fst
stsRHS   = map snd
allstsCT = concat . stsRHS


ndfa2ct :: Ord st => Ndfa st sy -> CT st
ndfa2ct (Ndfa v q s z delta) = limit (ndfa2dfaStep delta v) ttFstRow
  where  ttFstRow = consRows delta [epsilon_closure delta s] v


ndfa2dfaStep :: Ord st => (st -> (Maybe sy) -> [st]) -> [sy] -> CT st -> CT st
ndfa2dfaStep delta alfabet ct = nub (ct `union` consRows delta newSts alfabet)
  where newSts =  ((nub . allstsCT) ct) <-> (stsDfa ct)


consRows :: Ord st => (st -> (Maybe sy) -> [st]) -> [StDfa st] -> [sy] -> CT st
consRows delta []     alfabet = []
consRows delta (q:qs) alfabet = (q , oneRow delta q alfabet) :
                                (consRows delta qs alfabet)


oneRow :: Ord st => (st -> (Maybe sy) -> [st]) -> (StDfa st) -> [sy] -> [StDfa st]
oneRow delta sts alfabet = map (\ v -> sort (ndfawalk delta sts [v])) alfabet

ndfa2dfa :: (Ord st,Eq sy) => Ndfa st sy -> Dfa [st] sy
ndfa2dfa ndfa@(Ndfa v q s z delta)  = (Dfa v' q' s' z' delta')
  where  tt = ndfa2ct ndfa
         v' = v
         q' = stsDfa tt
         s' = fst (head tt)
         z' = finalStatesDfa q' z
         delta' st sy = lookupCT st sy tt v

finalStatesDfa :: Eq st => [StDfa st] -> [st] -> [StDfa st]
finalStatesDfa []     z = []
finalStatesDfa (q:qs) z | (q `intersect` z /= []) = q : finalStatesDfa qs z
                        | otherwise               = finalStatesDfa qs z

-- lookupCT :: (Eq st, Eq sy) => [st] -> sy -> CT st -> [sy] -> StDfa st
lookupCT st sy []     v  = []
lookupCT st sy (q:qs) v  | (fst q == st) = (snd q) !! col
                         | otherwise     = lookupCT st sy qs v
   where (Just col) = elemIndex sy v


--
-- Making a NDFA from a DFA
--

dfa2ndfa :: Dfa st sy -> Ndfa st sy
dfa2ndfa (Dfa v q s z delta) = (Ndfa v q [s] z delta')
  where delta' q (Just a) = [delta q a]
        delta' q Nothing  = []


--
-- Concatenation of Ndfa's
--

concatNdfa :: (Eq a, Eq b) => Ndfa b a -> Ndfa b a -> Ndfa b a
concatNdfa (Ndfa vp qp sp zp dp) (Ndfa vq qq sq zq dq) = Ndfa v' q' s' z' d'
  where v' = vp `union` vq
        q' = qp `union` qq
        s' = sp
        z' = zq
        d' q | q `elem` zp = dp' q
             | q `elem` qp = dp  q
             | otherwise   = dq  q
         where dp' q Nothing = (dp q Nothing) `union` sq
               dp' q sy      = dp q sy

--
-- Union Ndfa
--

unionNdfa :: (Eq a, Eq b) => Ndfa b a -> Ndfa b a -> Ndfa b a
unionNdfa (Ndfa vp qp sp zp dp) (Ndfa vq qq sq zq dq) = Ndfa v' q' s' z' d'
  where v' = vp `union` vq
        q' = qp `union` qq
        s' = sp `union` sq
        z' = zp `union` zq
        d' q | q `elem` qp = dp q
             | q `elem` qq = dq q

--
-- Star Ndfa
--

starNdfa :: Eq st => Ndfa st sy -> Ndfa st sy
starNdfa (Ndfa v qs s z d) = Ndfa v qs s z d'
  where d' q | q `elem` s = ds' q
             | q `elem` z = dz' q
             | otherwise  = d q
          where ds' q Nothing  = z `union` (d q Nothing)
                ds' q sy       = d q sy

                dz' q Nothing  = s `union` (d q Nothing)
                dz' q sy       = d q sy

--
-- Plus Ndfa
--

plusNdfa :: Eq st => Ndfa st sy -> Ndfa st sy
plusNdfa (Ndfa v qs s z d) = Ndfa v qs s z d'
  where d' q | q `elem` z = dz' q
             | otherwise  = d q
          where dz' q Nothing  = s `union` (d q Nothing)
                dz' q sy       = d q sy

--
-- Exponenciation
--

expNdfa :: (Eq st,Eq sy) => Ndfa st sy -> Int -> Ndfa Int sy
expNdfa ndfa n = expNdfa' (renameNdfa ndfa 1) n

expNdfa' :: Eq sy => Ndfa Int sy -> Int -> Ndfa Int sy
expNdfa'  ndfa 1 = ndfa
expNdfa'  ndfa i = concatNdfa ndfa (expNdfa' ndfa (i-1))

--
-- Concatenation of Dfa's
--

concatDfa :: (Eq a, Eq b) => Dfa b a -> Dfa b a -> Ndfa b a
concatDfa (Dfa vp qp sp zp dp) (Dfa vq qq sq zq dq) = Ndfa v' q' s' z' d'
  where v' = vp `union` vq
        s' = [sp]
        z' = zq
        q' = qp `union` qq
        d' q | q `elem` zp = dz' q
             | q `elem` qp = dp' q
             | q `elem` qq = dq' q
         where dz' q  Nothing = [sq]
               dz' q (Just y) | y `elem` vp = [dp q y]
                              | otherwise   = []

               dp' q Nothing  = []
               dp' q (Just y) | y `elem` vp = [dp q y]
                              | otherwise   = []

               dq' q Nothing  = []
               dq' q (Just y) | y `elem` vq = [dq q y]
                              | otherwise   = []

--
-- Union of Dfa's
--

unionDfa :: (Eq a, Eq b) => Dfa b a -> Dfa b a -> Ndfa b a
unionDfa (Dfa vp qp sp zp dp) (Dfa vq qq sq zq dq) = Ndfa v' q' s' z' d'
  where v' = vp `union` vq
        q' = qp `union` qq
        s' = [sp,sq]
        z' = zp ++ zq
        d' _ Nothing                 = []
        d' q (Just sy) | q `elem` qp && sy `elem` vp = [dp q sy]
                       | q `elem` qq && sy `elem` vq = [dq q sy]
                       | otherwise   = []

--
-- Star Dfa
--

starDfa :: Eq st => Dfa st sy -> Ndfa st sy
starDfa (Dfa v q s z d) = Ndfa v q [s] z d'
  where d' q | q == s     = ds' q
             | q `elem` z = dz' q
             | otherwise  = dd' q
          where ds' q Nothing  = z
                ds' q (Just y) = [d q y]

                dz' q Nothing  = [s]
                dz' q (Just y) = [d q y]

                dd' q (Just y) = [d q y]
                dd' _ _        = []

--
-- Plus Dfa
--

plusDfa :: Eq st => Dfa st sy -> Ndfa st sy
plusDfa (Dfa v q s z d) = Ndfa v q [s] z d'
  where d' q | q `elem` z = dz' q
             | otherwise  = dd' q
          where dz' q Nothing  = [s]
                dz' q (Just y) = [d q y]

                dd' q (Just y) = [d q y]
                dd' _ _ = []
