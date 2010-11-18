{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module SumSqVect (sumSq)
where

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Int as I

import qualified Prelude

sumSq :: Int -> Int
{-# NOINLINE sumSq #-}
sumSq n = I.sumP (mapP (\x -> x * x) (enumFromToP 1 n))
--sumSq n = I.sumP [:x * x | x <- [:1..n:]:]
  -- complains about: Variable not vectorised: GHC.PArr.$senumFromToP
