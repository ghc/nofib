{-
   **************************************************************
   * Filename      : RunTransducer.hs                           *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 88                                         *
   **************************************************************
-}

module FST.RunTransducer ( applyUp,
                       applyDown
                      ) where

import FST.Transducer

import Data.Maybe (catMaybes)

type TransitionFunction a = (Transducer a -> (State,Symbol a) ->
                                             [(Symbol a,State)])

applyUp :: Eq a => Transducer a -> [a] -> Maybe [[a]]
applyUp transducer input
 = apply transducer transitionsD input (initial transducer) []

applyDown :: Eq a => Transducer a -> [a] -> Maybe [[a]]
applyDown transducer input
 = apply transducer transitionsU input (initial transducer) []

apply :: Eq a => Transducer a -> TransitionFunction a -> [a] -> State ->
                 [Symbol a] -> Maybe [[a]]
apply transducer transFun input s  result =
 case (runEpsilon transducer transFun input s result,
       runSymbol transducer transFun input s result) of
   (Just xs, Just ys) -> Just $ xs ++ ys
   (a, Nothing)       -> a
   (Nothing, b)       -> b

runEpsilon :: Eq a => Transducer a -> TransitionFunction a -> [a] -> State ->
                 [Symbol a] -> Maybe [[a]]
runEpsilon transducer transFun input s result =
 case (transFun transducer (s,Eps)) of
  [] -> Nothing
  tl -> case (concat $ catMaybes $
         map (\(a,s1) -> apply transducer transFun input s1 (a:result)) tl) of
         [] -> Nothing
         xs -> return xs

runSymbol :: Eq a => Transducer a -> TransitionFunction a -> [a] -> State ->
                 [Symbol a] -> Maybe [[a]]
runSymbol transducer _ [] s result
 | isFinal transducer s = return [transform result]
 | otherwise            = Nothing
runSymbol transducer transFun (i:input) s result =
 case (transFun transducer (s,S i)) of
  [] -> Nothing
  tl -> case (concat $ catMaybes $
         map (\(a,s1) -> apply transducer transFun input s1 (a:result)) tl) of
         [] -> Nothing
         xs -> return xs

transform :: [Symbol a] -> [a]
transform ys = transform' ys []
 where transform' []         res = res
       transform' ((S a):xs) res = transform' xs (a:res)
       transform' ((_:xs))   res = transform' xs res
