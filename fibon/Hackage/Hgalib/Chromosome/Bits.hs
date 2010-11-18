{-# OPTIONS_GHC -fglasgow-exts #-}

-- | Chromosomes represented as a bit field
module Chromosome.Bits (mutateBits,
                        bits2int,
                        randomBits,
                        pointCross,
                        config)
where

import List
import Random
import Control.Monad.State.Strict
import GA

-- |The config for a chromosome of a list of bits. User must defined fitness and mutate.
config :: ChromosomeConfig [a] p
config = ChromosomeConfig {
           fitness = undefined,
           mutate = undefined,
           cross = pointCross
}

-- |Single point cross at a random location
pointCross :: [a] -> [a] -> (GAState c p) ([a],[a])
pointCross xs ys = do
  let len = length xs
  point <- gaRand (0, len)
  let (left1, right1) = splitAt point xs
      (left2, right2) = splitAt point ys
      in return $ (left1 ++ right2, left2 ++ right1)

-- |Generates i random bits
randomBits :: Int -> (GAState c p) [Bool]
randomBits i = replicateM i (gaRand (True, False))

-- |Randomly flips fits with a specified probability
mutateBits :: Double -> [Bool] -> (GAState c p) [Bool]
mutateBits mutationRate xs =
    mapM (mutateBit mutationRate) xs


mutateBit r b = do
  test <- gaRand (0.0, 1.0)
  if test < r
     then return $ not b
     else return b

-- |Converts a list of Bool's to it's integer representation
bits2int :: [Bool] -> Int
bits2int bs =
    sum [ x | (x, b) <- zip _2pwrs bs, b ]
    where _2pwrs = 1 : map (*2) _2pwrs