{-# OPTIONS_GHC -fglasgow-exts #-}

-- | Genetic Algorithms
module GA (Config (..),
           PopulationConfig (..),
           ChromosomeConfig (..),
           defaultConfig,
           GAState,
           bestChromosome,
           gaRand,
           run,
           rouletteM,
           mutateM,
           crossM,
           tournamentM,
           isDone)
where

import Data.Maybe
import System.Random
import Control.Monad.State.Strict

type GAState c p = State (Config c p)

data Config c p = Config {
      -- |The config for the chromosome model
      cConfig :: ChromosomeConfig c p,
      -- |The config for the population model
      pConfig :: PopulationConfig c p,
      -- |The function that transforms a population into the next generation
      newPopulation :: p -> (GAState c p) p,
      -- |The fitness at which to stop the GA
      maxFitness :: Maybe Double,
      -- |The generation at which to stop the GA
      maxGeneration :: Maybe Int,
      -- |The number of generations elapsed. defaultConfig sets this to 0
      currentGeneration :: Int,
      -- |The random number generator
      gen :: StdGen
      }

data ChromosomeConfig c p = ChromosomeConfig {
      -- |The fitness function for the chromosome model
      fitness :: c -> Double,
      -- |The mutation operator for the chromosome model
      mutate :: c -> (GAState c p) c,
      -- |The crossover operator for the chromosome model
      cross :: c -> c -> (GAState c p) (c,c)
      }

data PopulationConfig c p = PopulationConfig {
      bestChromosomePop :: p -> (GAState c p) c,
      roulettePop :: p -> (GAState c p) p,
      tournamentPop :: p -> (GAState c p) p,
      applyCrossoverPop :: p -> (GAState c p) p,
      applyMutationPop :: p -> (GAState c p) p
      }
-- |defaultConfig acts as a blank slate for genetic algorithms.
-- cConfig, pConfig, gen, and maxFitness or maxGeneration must be defined
defaultConfig :: Config c p
defaultConfig = Config {
                  cConfig = undefined,
                  pConfig = undefined,
                  newPopulation = undefined,
                  maxFitness = Nothing,
                  maxGeneration = Nothing,
                  currentGeneration = 0,
                  gen = undefined
                  }

-- |Wrapper function which returns the best chromosome of a population
bestChromosome :: p -> (GAState c p) c
bestChromosome pop = do
  config <- get
  bestChromosomePop (pConfig config) pop

-- |Wrapper function which returns the highest-fitness member of a population
highestFitness :: p -> (GAState c p) Double
highestFitness pop = do
  fitFunc <- (fitness . cConfig) `liftM` get
  best <- bestChromosome pop
  return $ fitFunc best

-- |A wrapper function for use in newPopulation for roulette selection
rouletteM :: p -> (GAState c p) p
rouletteM pop =
  (roulettePop . pConfig) `liftM` get >>= ($pop)

-- |A wrapper function for use in newPopulation for tournament selection
tournamentM :: p -> (GAState c p) p
tournamentM pop =
    (tournamentPop . pConfig) `liftM` get >>= ($pop)

-- |A wrapper function for use in newPopulation for mutating the population
mutateM :: p -> (GAState c p) p
mutateM pop = do
  (applyMutationPop . pConfig) `liftM` get >>= ($pop)

-- |A wrapper function for use in newPopulation for applying crossover to the population
crossM :: p -> (GAState c p) p
crossM pop =
    (applyCrossoverPop . pConfig) `liftM` get >>= ($pop)

newPopulationM :: p -> (GAState c p) p
newPopulationM pop =
    incGA >> newPopulation `liftM` get >>= ($pop)

incGA :: (GAState c p) ()
incGA = modify (\c@Config { currentGeneration = g} ->
                    c { currentGeneration = g + 1})

untilM :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
untilM p f x = do
  test <- p x
  if test 
     then return x
     else f x >>= untilM p f

-- |Runs the specified GA config until the termination condition is reached
run :: p -> (GAState c p) p
run = untilM isDone newPopulationM

-- |Returns true if the given population satisfies the termination condition for the GA config
isDone :: p -> (GAState c p) Bool
isDone population = do
  c <- get
  f <- highestFitness population
  let generationsDone =
          maybe False (<(currentGeneration c)) $ maxGeneration c
  let fitnessDone =
          maybe False (>f) $ maxFitness c
  return $ generationsDone || fitnessDone

-- |Generates a random number which updating the random number generator for the config
gaRand :: (Random a) =>
          (a,a) -> (GAState c p) a
gaRand range = do
  config <- get
  let g = gen config
  let (x, g') = randomR range g
  put $ config { gen = g' }
  return x
