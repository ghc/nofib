
-- | Populations represented as a diff array of chromosomes
module Population.Array (config, fromList)
where

import GA

import Data.Array.Diff
import Control.Monad
import Control.Monad.State.Strict
import Data.List

-- |The type used to represent population arrays; is a diff array.
type PArray c = DiffArray Int c

-- |Population config for arrays
config :: PopulationConfig c (PArray c)
config = PopulationConfig {
           bestChromosomePop = bestChromosomeArray,
           roulettePop = rouletteArray,
           tournamentPop = tournamentArray,
           applyCrossoverPop = crossoverArray,
           applyMutationPop = mutateArray
}

--yipee
tournamentArray :: PArray c -> (GAState c p) (PArray c)
tournamentArray arr = do
  f <- (fitness . cConfig) `liftM` get
  let augument c = (c, f c)
  let augArr = amap augument arr
  aforM arr $ \_ -> do
    index1 <- gaRand $ bounds arr
    index2 <- gaRand $ bounds arr
    let chrom1 = augArr ! index1
    let chrom2 = augArr ! index2
    if snd chrom1 > snd chrom2
       then return $ fst chrom1
       else return $ fst chrom2

mutateArray :: PArray c -> (GAState c (PArray c)) (PArray c)
mutateArray arr =
    (mutate . cConfig) `liftM` get >>= flip amapM arr

crossoverArray arr = do
  c <- (cross . cConfig) `liftM` get
  crossoverArray' c arr $ fst $ bounds arr

crossoverArray' cross arr index =
    let lastIndex = snd $ bounds arr in
    if index > lastIndex
       then return arr
       else
           let p1 = arr ! index
               p2 = arr ! (index + 1)
           in do (c1, c2) <- cross p1 p2
                 let newArr = arr // [(index, c1), (index + 1, c2)]
                 crossoverArray' cross newArr (index + 2)

bestChromosomeArray :: PArray c -> (GAState c p) c
bestChromosomeArray arr = do
    f <- (fitness . cConfig) `liftM` get
    let cmp x y | f x > f y = x
                | otherwise = y
    return $ afoldl1 cmp arr


rouletteArray :: PArray c -> (GAState c p) (PArray c)
rouletteArray arr = do
  f <- (fitness . cConfig) `liftM` get
  let totalFitness = (afoldl (\fit c -> fit + f c) 0.0 arr) :: Double
  let augument chrom = (chrom, (f chrom) / totalFitness)
  let augArr = amap augument arr
  aforM arr $ \_ ->
      selectDistribution augArr 0.0 $ fst $ bounds arr

selectDistribution :: PArray (c, Double) -> Double -> Int -> (GAState c p) c
selectDistribution arr acc index =
    let lastIndex = snd $ bounds arr in
    if index == lastIndex
       then return $ fst $ arr ! lastIndex
       else do
         let (chrom, fit) = arr ! index
         test <- gaRand (0.0, 1.0)
         if test < fit / (1 - acc)
            then return chrom
            else selectDistribution arr (fit + acc) (index + 1)

-- |Converts a list to an array
fromList xs = listArray (0, length xs - 1) xs

amapM p arr =
    listArray (bounds arr) `liftM` mapM p (elems arr)

aforM arr p = amapM p arr
 
afoldl p seed arr =
    foldl p seed $ elems arr

afoldl1 p arr =
    foldl1 p $ elems arr
