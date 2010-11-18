{-# OPTIONS_GHC -fexcess-precision #-}
--
-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- Translation from Clean by Don Stewart
--
-- Should be compiled with:
--      -O -fglasgow-exts -fbang-patterns 
--      -optc-O -optc-march=pentium4 -optc-mfpmath=sse -optc-msse2
--

import System.Environment
import Foreign.Marshal.Array
import Foreign
import Text.Printf
import Control.Monad
import Data.ByteString.Internal (inlinePerformIO)

type Reals = Ptr Double

main = do
    n <- getArgs >>= readIO . head

    u <- mallocArray n :: IO Reals
    mapM_ (\i -> pokeElemOff u i 1)[0..n-1] 
    v <- mallocArray n :: IO Reals
    mapM_ (\i -> pokeElemOff v i 0)[0..n-1] 

    powerMethod 10 n u v
    printf "%.2f\n" (eigenvalue n u v 0 0 0)

    return ()

------------------------------------------------------------------------

eigenvalue :: Int -> Reals -> Reals -> Int -> Double -> Double -> Double
eigenvalue n u v i vBv vv
    | n `seq` u `seq` v `seq` i `seq` vBv `seq` vv `seq` False = undefined
    | i < n     = eigenvalue n u v (i+1) (vBv + ui * vi) (vv + vi * vi)
    | otherwise = sqrt $! vBv / vv
    where
       ui = inlinePerformIO (peekElemOff u i)
       vi = inlinePerformIO (peekElemOff v i)

------------------------------------------------------------------------

powerMethod :: Int -> Int -> Reals -> Reals -> IO ()
powerMethod i n u v | i `seq` n `seq` u `seq` v `seq` False = undefined
powerMethod i n u v = allocaArray n $ \t ->
    replicateM_ i $ timesAtAv t n u v >> timesAtAv t n v u

-- multiply vector v by matrix A and then by matrix A transposed
timesAtAv :: Reals -> Int -> Reals -> Reals -> IO ()
timesAtAv t n u atau | t `seq` n `seq` u `seq` atau `seq` False = undefined
timesAtAv t n u atau = do
    timesAv  n u t
    timesAtv n t atau
{-# INLINE timesAtAv #-}

timesAv :: Int -> Reals -> Reals -> IO ()
timesAv n u au | n `seq` u `seq` au `seq` False = undefined
timesAv n u au = go 0
  where
    go :: Int -> IO ()
    go i = i `seq` when (i < n) $ do
        pokeElemOff au i (avsum i 0 0)
        go (i+1)

    avsum :: Int -> Int -> Double -> Double
    avsum i j acc
        | i `seq` j `seq` acc `seq` False = undefined
        | j < n = avsum i (j+1) (acc + ((aij i j) * uj))
        | otherwise = acc
        where uj = inlinePerformIO (peekElemOff u j)
{-# INLINE timesAv #-}

timesAtv :: Int -> Reals -> Reals -> IO ()
timesAtv n u a | n `seq` u `seq` a `seq` False = undefined
timesAtv n u a = go 0
  where
    go :: Int -> IO ()
    go i = i `seq` when (i < n) $ do
        pokeElemOff a i (atvsum i 0 0)
        go (i+1)

    atvsum :: Int -> Int -> Double -> Double
    atvsum i j acc
        | i `seq` j `seq` acc `seq` False = undefined
        | j < n     = atvsum i (j+1) (acc + ((aij j i) * uj))
        | otherwise = acc
        where uj = inlinePerformIO (peekElemOff u j)
{-# INLINE timesAtv #-}

--
-- manually unbox the inner loop:
aij i j = 1 / fromIntegral ((i+j) * (i+j+1) `div` 2 + i + 1)
