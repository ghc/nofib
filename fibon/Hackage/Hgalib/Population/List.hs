{-# OPTIONS_GHC -fglasgow-exts #-}

-- | Populations represented as a list of chromosomes
-- Arrays are recommended instead for performance reasons.
module Population.List (config)
where

import GA

import Control.Monad
import Control.Monad.State.Strict
import Random
import List

-- |Config for use of lists as the population model. Lists are deprecated in favor of arrays.
config :: PopulationConfig c [c]
config = PopulationConfig {
           bestChromosomePop = bestChromosomeList,
           roulettePop = rouletteList,
           tournamentPop = tournamentList,
           applyCrossoverPop = crossoverList,
           applyMutationPop = mutateList
           }

-- Warning: shit performance for tournamentList :/ O(n^2) afaict
-- Moral o Story: Lists suck, but arrays are fugly ;_;
tournamentList :: [c] -> (GAState c [c]) [c]
tournamentList xs = do
  f <- (fitness . cConfig) `liftM` get
  let len = length xs
  let augChroms = map (\c -> (c, f c)) xs
  forM xs $ \_ -> do
    index1 <- gaRand (0, len - 1)
    index2 <- gaRand (0, len - 1)
    let test1 = augChroms !! index1
    let test2 = augChroms !! index2
    if snd test1 > snd test2
       then return $ fst test1
       else return $ fst test2

crossoverList :: [c] -> (GAState c [c]) [c]
crossoverList [] = return []
crossoverList [x] = return [x]
crossoverList (x:y:xs) = do
  c <- (cross . cConfig) `liftM` get
  (offspring1,offspring2) <- c x y
  rest <- crossoverList xs
  return $ offspring1 : offspring2 : rest

mutateList :: [c] -> (GAState c [c]) [c]
mutateList cs =
    (mutate . cConfig) `liftM` get >>= forM cs

rouletteList :: [c] -> (GAState c [c]) [c]
rouletteList cs = do
  f <- (fitness . cConfig) `liftM` get
  let fs = map f cs
  let total = sum fs
  let probs = map (/total) fs
  let augumentedChromosomes = zip cs probs
  forM cs $
           \_ -> selectDistribution augumentedChromosomes


bestChromosomeList :: [c] -> (GAState c [c]) c
bestChromosomeList cs = do
  f <- (fitness . cConfig) `liftM` get
  let compareChromosomes x y =
          compare (f x) (f y)
  return $ maximumBy compareChromosomes cs


selectDistribution :: [(a, Double)] -> (GAState c p) a
selectDistribution xs =
    select 0.0 xs
    where select _ ((a,p):[]) = return a
          select acc ((a,p):xs) = do
            test <- gaRand (0,1.0)
            if test < p / (1 - acc)
               then return a
               else select (p + acc) xs