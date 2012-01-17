import GA
import qualified Population.List as L
import System.Random
import Control.Monad.State.Strict
import Data.List
import qualified Chromosome.ANN as ANN
import Control.Monad


-- This function takes a population ("pop"), then mutates it, then applies roulette selection
mynewPopulation pop = mutateM pop >>= rouletteM


myconfig = defaultConfig {
             -- cConfig configures the chromosome model and the genetic operators
             cConfig = ANN.config {
                         fitness = ANN.fitnessMSE xorExamples,
                         mutate = ANN.mutateShift 0.1 1.0,
                         cross = ANN.uniformCross
                         },

             -- The population will be represented as a list
             pConfig = L.config,

             -- To generate the next population, mynewPopulation will be called
             newPopulation = mynewPopulation,

             -- Stop after 1000 generations
             maxGeneration = Just 1000,

             -- Initialize the random number generator to 42
             -- 42 is chosen for obvious reasons
             gen = mkStdGen 42
             }

-- Create an initial population of 20 ANN's
-- Each will have 2 inputs and two hidden layers of [2,1] nodes, respectively
-- The weights will be randomly initialized to +/- 2.0
initPop = replicateM 20 $ ANN.randomANN 2 [2,1] 2.0

-- The examples are a list of (in, out)
xorExamples =
    zip xorIn xorOut
    where xorIn = [[0.0,0.0], [1.0,1.0], [1.0,0.0], [0.0,1.0]]
          xorOut = [[0.0], [0.0], [1.0], [1.0]]

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
  -- Run the GA with the config "myconfig"
  let (before, after) = evalState getBest myconfig
  
  -- Tell how many examples are correct within +/0 0.3
  putStrLn $ show $ round $ ANN.correctExamples xorExamples 0.3 before
  putStrLn $ show $ round $ ANN.correctExamples xorExamples 0.3 after
  
  -- Print out the final neural network
  putStrLn $ show $ after
