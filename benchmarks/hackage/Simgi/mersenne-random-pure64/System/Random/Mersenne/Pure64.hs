{-# LANGUAGE CPP, ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module     : System.Random.Mersenne.Pure64
-- Copyright  : Copyright (c) 2008, Don Stewart <dons@galois.com>
-- License    : BSD3
-- Maintainer : Don Stewart <dons@galois.com>
-- Stability  : experimental
-- Portability: CPP, FFI
-- Tested with: GHC 6.8.3
--
-- A purely functional binding 64 bit binding to the classic mersenne
-- twister random number generator. This is more flexible than the
-- impure 'mersenne-random' library, at the cost of being a bit slower.
-- This generator is however, many times faster than System.Random,
-- and yields high quality randoms with a long period.
--
-- This generator may be used with System.Random, however, that is
-- likely to be slower than using it directly.
--
module System.Random.Mersenne.Pure64 (

    -- * The random number generator
    PureMT          -- abstract: RandomGen

    -- * Introduction
    , pureMT        -- :: Word64 -> PureMT
    , newPureMT     -- :: IO PureMT

    -- $instance

    -- * Low level access to the generator

    -- $notes
    , randomInt     -- :: PureMT -> (Int   ,PureMT)
    , randomWord    -- :: PureMT -> (Word  ,PureMT)
    , randomInt64   -- :: PureMT -> (Int64 ,PureMT)
    , randomWord64  -- :: PureMT -> (Word64,PureMT)
    , randomDouble  -- :: PureMT -> (Double,PureMT)

    ) where

------------------------------------------------------------------------

import System.Random.Mersenne.Pure64.MTBlock
import System.Random
import Data.Word
import Data.Int
import System.Time
import System.CPUTime

-- | Create a PureMT generator from a 'Word64' seed.
pureMT :: Word64 -> PureMT
pureMT = mkPureMT . seedBlock . fromIntegral

-- | Create a new PureMT generator, using the clocktime as the base for the seed.
newPureMT :: IO PureMT
newPureMT = do
    ct             <- getCPUTime
    (TOD sec psec) <- getClockTime
    return $ pureMT (fromIntegral $ sec * 1013904242 + psec + ct)

------------------------------------------------------------------------
-- System.Random interface.

-- $instance
--
-- Being purely functional, the PureMT generator is an instance of
-- RandomGen. However, it doesn't support 'split' yet.

#if __GLASGOW_HASKELL__ < 700
instance RandomGen PureMT where
   next  = randomInt
   split = error "System.Random.Mersenne.Pure: unable to split the mersenne twister"
#else
instance RandomGen PureMT where
   next  = randomInt
#endif
------------------------------------------------------------------------
-- Direct access to Int, Word and Double types

-- | Yield a new 'Int' value from the generator, returning a new
-- generator and that 'Int'. The full 64 bits will be used on a 64 bit machine.
randomInt :: PureMT -> (Int,PureMT)
randomInt g = (fromIntegral i, g')
        where (i, g') = randomWord64 g
{-# INLINE randomInt #-}

-- | Yield a new 'Word' value from the generator, returning a new
-- generator and that 'Word'.
randomWord :: PureMT -> (Word,PureMT)
randomWord g = (fromIntegral i, g')
        where (i, g') = randomWord64 g
{-# INLINE randomWord #-}

-- | Yield a new 'Int64' value from the generator, returning a new
-- generator and that 'Int64'.
randomInt64 :: PureMT -> (Int64,PureMT)
randomInt64 g = (fromIntegral i, g')
        where (i, g') = randomWord64 g
{-# INLINE randomInt64 #-}

-- | Efficiently yield a new 53-bit precise 'Double' value, and a new generator.
randomDouble :: PureMT -> (Double,PureMT)
randomDouble g = (fromIntegral (i `div` 2048) / 9007199254740992, g')
        where (i, g') = randomWord64 g
{-# INLINE randomDouble #-}

-- | Yield a new 'Word64' value from the generator, returning a new
-- generator and that 'Word64'.
randomWord64 :: PureMT -> (Word64,PureMT)
randomWord64 (PureMT block i nxt) = (mixWord64 (block `lookupBlock` i), mt)
  where
    mt | i < blockLen-1 = PureMT block (i+1) nxt
       | otherwise      = mkPureMT nxt
{-# INLINE randomWord64 #-}

-- | 'PureMT', a pure mersenne twister pseudo-random number generator
--
data PureMT  = PureMT {-# UNPACK #-} !MTBlock {-# UNPACK #-} !Int MTBlock

instance Show PureMT where
    show _ = show "<PureMT>"

-- create a new PureMT from an MTBlock
mkPureMT :: MTBlock -> PureMT
mkPureMT block = PureMT block 0 (nextBlock block)
