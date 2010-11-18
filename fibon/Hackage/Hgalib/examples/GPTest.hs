import GA
import qualified Population.List as L
import Random
import Control.Monad.State.Strict
import List
import qualified Chromosome.GP as GP
import Control.Monad
import System.Environment

-- Defines the operators available for the GP
-- You must specify the function, then the arity (number of arguments), then the textual representation
ops :: [GP.Op Double Double]
ops = [
 GP.Op (\[x,y] -> return $ x + y) 2 "+", -- Addition
 GP.Op (\[x,y] -> return $ x - y) 2 "-", -- Subtraction
 GP.Op (\[x,y] -> return $ x * y) 2 "*", -- Multiplication
 GP.Op (\[] -> return 1) 0 "1", -- The constant 1
 GP.Op (\[] -> get) 0 "x" -- The independant variable
 ]

-- Examples for the function f(x) = 3x^2 + 1
examples :: [(Double,Double)]
examples = zip [1..10] $ map (\x -> 3 * x * x + 1) [1..10]

myconfig = defaultConfig {
             -- cConfig configures the chromosome model and the genetic operators
             cConfig = ChromosomeConfig {
                         -- Compute fitness based on the mean square error across the examples
                         fitness = GP.mseFitness examples,
                         -- Max tree depth of 4, mutation rate of 0.05
                         mutate = GP.mutate 8 ops (0.05 :: Double),
                         -- Oops! crossover not yet implemented for GP, see GP.hs for details
                         cross = error "Attempt to cross GP"
                         },
             
             -- The population will be represented as a list
             pConfig = L.config,

             -- To generate the next population, mynewPopulation will be called
             newPopulation = mynewPopulation,
             
             -- Stop after 100 generations
             maxGeneration = Just 500,
             
             -- Initialize the random number generator to 42
             -- 42 is chosen for obvious reasons
             gen = mkStdGen 42
             }
-- This function takes a population ("pop"), then mutates it, then applies roulette selection
mynewPopulation pop = mutateM pop >>= rouletteM

-- Create an initial population of 100 GP's 
-- Tree depth of 4 using ops as the pool of available tree nodes
initPop = replicateM 100 $ GP.random 4 ops

getBest = do
  pop <- initPop
  
  -- Grab the best chromosome at the start for comparision
  before <- bestChromosome pop
  
  -- Evaluation of the GA
  answer <- run pop
  
  -- Find the new best chromosome after running the GA
  after <- bestChromosome answer
  
  return (before, after)

main = do
  [nGens] <- getArgs
  let numGens = Just $ read nGens
  -- Run the GA with the config "myconfig"
  let (before,after) = evalState getBest (myconfig {maxGeneration = numGens})
  
  -- Show the before/after s-expressions
  putStrLn $ show $ before
  putStrLn $ show $ after
