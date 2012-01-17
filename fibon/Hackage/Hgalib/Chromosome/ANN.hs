{-# OPTIONS_GHC -fglasgow-exts #-}

-- | Artificial Neural Networks
module Chromosome.ANN (ANN, Layer, Node,
                       eval,
                       config,
                       uniformCross,
                       averageCross,
                       mutateRandomize,
                       mutateShift,
                       fitnessMSE,
                       averageMSE,
                       correctExamples,
                       randomANN)
where

import GA
import Control.Monad.State.Strict
import Data.List
import System.Random

-- |An Artificial Neural Network
type ANN = [Layer]
-- |A layer of nodes in an ANN
type Layer = [Node]
-- |A node in an ANN. The head of the list is the bias weight. The tail is the weights for the previous layer
type Node = [Double]

-- User must specify fitness, mutate, and crossover functions
config = ChromosomeConfig {
           fitness = undefined,
           mutate = undefined,
           cross = undefined
           }

-- |Returns the number of examples correct within the tolerance. The examples are a list of tuples of (input, output)
correctExamples :: [([Double],[Double])] -> Double -> ANN -> Double
correctExamples examples tolerance ann =
    fromIntegral $ sum $ map (correctExample ann tolerance) examples

correctExample :: ANN -> Double -> ([Double],[Double]) -> Int
correctExample ann tolerance example =
    numMatching ((<tolerance) . abs) $
                rawError ann example

-- |Computes the fitness based on the mean square error for a list of examples
-- The examples are a list of tuples of (input, output)
fitnessMSE :: [([Double],[Double])] -> ANN -> Double
fitnessMSE examples ann = 1.0 / averageMSE ann examples

-- |Computes the mean square error for a list of examples
-- The examples are a list of tuples of (input, output)
averageMSE :: ANN -> [([Double],[Double])] -> Double
averageMSE ann examples =
    average $ map (mse ann) examples

mse :: ANN -> ([Double],[Double]) -> Double
mse ann examples =
    average $ map (^2) $ rawError ann examples

rawError :: ANN -> ([Double], [Double]) -> [Double]
rawError ann (ins, outs) =
    zipWith (-) outs $ eval ins ann

-- |Mutates an ANN by randomly settings weights to +/- range
mutateRandomize :: Double -> Double -> ANN -> (GAState ANN p) ANN
mutateRandomize rate range ann =
    mapM (mapM (mapM rnd)) ann
    where rnd = randWeight False rate range

-- |Mutates an ANN by randomly shifting weights by +/- range
mutateShift :: Double -> Double -> ANN -> (GAState ANN p) ANN
mutateShift rate range ann =
    mapM (mapM (mapM rnd)) ann
    where rnd = randWeight True rate range

randWeight :: Bool -> Double -> Double -> Double -> (GAState c p) Double
randWeight shiftp rate range weight = do
  test <- gaRand (0.0, 1.0)
  if test > rate
     then return weight
     else do
       delta <- gaRand (-range, range)
       return $ delta + (if shiftp then weight else 0.0)

-- |Crossover between two ANN's by exchanging weights
uniformCross :: ANN -> ANN -> (GAState c p) (ANN,ANN)
uniformCross xsss ysss =
    zipWithM (zipWithM (zipWithM pickRandom)) xsss ysss >>=
    return . unzip . map unzip . map (map unzip)

-- |Crossover between two ANN's by averaging weights
averageCross :: ANN -> ANN -> (GAState c p) (ANN,ANN)
averageCross n1 n2 =
    let retval = zipWith (zipWith (zipWith avg)) n1 n2
    in return (retval, retval)

pickRandom :: a -> a -> (GAState c p) (a,a)
pickRandom x y = do
  test <- gaRand (False, True)
  if test then return (x,y) else return (y,x)

-- |Evaluates an ANN with a given input
eval :: [Double] -> ANN -> [Double]
eval input [] = input
eval input (x:xs) =
    eval (evalLayer input x) xs

evalLayer :: [Double] -> Layer -> [Double]
evalLayer inputs =
    map (evalNode inputs)

evalNode :: [Double] -> Node -> Double
evalNode inputs (bias : weights) =
    sigmoid $ bias + dotProduct inputs weights

-- |Generates a random ANN with a given number of input nodes, a list of number of hidden nodes per layer, and the weight range
randomANN :: Int -> [Int] -> Double -> (GAState c p) ANN
randomANN _ [] _ = return []
randomANN i (l:ls) r = do
  x <- randomLayer i l r
  xs <- randomANN l ls r
  return $ x : xs

randomLayer :: Int -> Int -> Double -> (GAState c p) Layer
randomLayer i o range = replicateM o $ randomNode i range

randomNode :: Int -> Double -> (GAState c p) Node
randomNode i range = replicateM (i+1) $ gaRand (-range,range)

-- Low level utilities
sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (-x))

dotProduct :: [Double] -> [Double] -> Double
dotProduct u v = sum $ zipWith (*) u v

avg x y = (x + y) / 2.0
average xs = sum xs / genericLength xs

numMatching p =
    foldl (\acc x -> if p x then acc + 1 else acc) 0
