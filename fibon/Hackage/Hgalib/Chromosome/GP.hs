{-# OPTIONS_GHC -fglasgow-exts #-}

-- | Genetic Programming as strictly-evaluated s-expressions
module Chromosome.GP (Op (..),
                      Node (..),
                      mseFitness,
                      mutate,
                      eval,
                      random,
                      config)
where

import Data.Array
import qualified GA
import Control.Monad.State.Strict
import Data.List

-- The config for the GP chromosome model. mutate, cross, and fitness must be defined.
config = GA.ChromosomeConfig {
           GA.mutate = undefined,
           GA.cross = undefined,
           GA.fitness = undefined
           }

-- |An operator in the syntax tree of the GP
data Op a s = Op {
      -- |The function for evaluating this node
      callback :: ([a] -> State s a),
      -- |The number of children of this node
      arity :: Int,
      -- |The name of the node when shown
      name :: String
      }

instance Show (Op a s) where
    show = name

-- |A node in the syntax tree of the GP
data Node a s = Node (Op a s) [Node a s]

instance Show (Node a s) where
    show (Node o children) =
        if arity o == 0
           then show o
           else "(" ++ (unwords $ show o : map show children) ++ ")"

-- |Calculates fitness based on the mean square error across a list of examples
-- The examples are a list of tuples of (inputs state, correct output)
mseFitness :: (Fractional a) => [(s, a)] -> Node a s -> a
mseFitness examples node =
    1.0 / (mse node examples + 1.0)

mse :: (Fractional a) => Node a s -> [(s, a)] -> a
mse node examples =
    average $ map (^2) $ map (\(i,o) -> delta node i o) examples

delta node state output =
    output - (evalState (eval node) state)

-- |Statefully evaluates a given GP
eval :: Node a s -> State s a
eval (Node (Op f _ _) ns) =
    mapM eval ns >>= f

-- |Mutates a GP by replacing nodes with random GP's
mutate 0 ops rate tree = error "Attempt to mutate with depth of 0"
mutate d ops rate tree@(Node op children) = do
  test <- GA.gaRand (0.0, 1.0)
  if test >= rate  -- No mutation
     then do newChildren <- mapM (mutate (d-1) ops rate) children
             return $ Node op newChildren
     else random d ops

-- |Generates a random GP with a given depth limit
random 0 ops = error "Attempt to create depth 0 tree"
random 1 ops = do
  op <- randomOp $ filter ((==0) . arity) ops
  return $ Node op []
random d ops = do
  op <- randomOp ops
  children <- replicateM (arity op) $ random (d-1) ops
  return $ Node op children

randomOp ops =
    GA.gaRand (0,length ops - 1) >>=
    return . (ops !!)

average xs = sum xs / genericLength xs
