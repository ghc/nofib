import GA
import qualified Population.List as L
import Random
import Control.Monad.State.Strict
import List
import qualified Chromosome.Bits as B
import Control.Monad

myconfig = defaultConfig {
             -- cConfig configures the chromosome model and the genetic operators
             cConfig = B.config {
                         -- This (trivial) fitness function just converts the bits to a double
                         -- Larger numbers are "more fit"; the maximally fit chromosome is all 1's (True's)
                         fitness = fromIntegral . B.bits2int,
                         mutate = B.mutateBits (0.1 :: Double)
                         -- The crossover operator defaults to point cross
                         -- This only works for some chromosomes, such as bits
                         },

             -- The population will be represented as a list
             pConfig = L.config,

             -- To generate the next population, mynewPopulation will be called
             newPopulation = mynewPopulation,
             
             -- Stop after 100 generations
             maxGeneration = Just 100,
             
             -- Initialize the random number generator to 42
             -- 42 is chosen for obvious reasons
             gen = mkStdGen 42
             }

-- This function takes a population ("pop"), then mutates it, then applies roulette selection
mynewPopulation pop = mutateM pop >>= rouletteM

-- Create an initial population of 100 bit lists
-- Each will have the 4 LSB randomly generated and the 4 MSB set to False
initPop = liftM (map (++[False,False,False,False])) $ replicateM 100 (B.randomBits 4)

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
  
  -- Show the starting and ending fitness
  putStrLn $ show $ round $ fromIntegral $ B.bits2int before
  putStrLn $ show $ round $ fromIntegral $ B.bits2int after

  -- Print out the final chromosome
  putStrLn $ show after
