{-# LANGUAGE BangPatterns #-}
-- A basic correctness and performance test for mersenne-random-pure64.
--
-- Copyright (c) 2008, Don Stewart <dons@galois.com>

import Control.Exception
import Control.Monad
import Data.Int
import Data.Typeable
import Data.Word
import System.CPUTime
import System.Environment
import System.IO
import Text.Printf
import qualified System.Random as Old
import qualified System.Random.Mersenne as Unsafe

import System.Random.Mersenne.Pure64
import System.Random.Mersenne.Pure64.Base
import Control.Concurrent
import Control.Concurrent.MVar

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

seed = 7

main = do

    c_init_genrand64_unsafe seed
    let g = pureMT (fromIntegral seed)

    ------------------------------------------------------------------------
    -- calibrate
    s <- newMVar 0 :: IO (MVar Int)
    putStr "Calibrating ... " >> hFlush stdout

    tid <- forkIO $ do
        let go !i !g = do
                let (!_, !g') = randomWord64 g
                x <- swapMVar s i
                x `seq` go (i+1) g'
        go 0 g

    threadDelay (1000 * 1000)
    killThread tid
    lim <- readMVar s -- 1 sec worth of generation
    putStrLn $ "done. Using N=" ++ show lim

    time $ do
        let m = 2*lim
        putStr $ "Checking against released mt19937-64.c to depth " ++ show m ++ " "
        hFlush stdout
        equivalent g m

    speed lim

    return ()

------------------------------------------------------------------------

equivalent !g !n | n > 0 = do

    i'      <- c_genrand64_int64_unsafe
    d'      <- c_genrand64_real2_unsafe

    let (i, g')  = randomWord64 g
        (d, g'') = randomDouble g'

    if i == fromIntegral i' && d == realToFrac d'
        then do when (n `rem` 500000 == 0) $ putChar '.' >> hFlush stdout
                equivalent g'' (n-1)

        else do print $ "Failed! " ++ show ((i,i') , (d,d'))
                return g''

equivalent g _ = do putStrLn "Matches model!"
                    return g

------------------------------------------------------------------------
-- compare with System.Random

-- overhead cause by random's badness
speed lim = do

 time $ do
    putStrLn $ "System.Random"
    let g = Old.mkStdGen 5
    let go :: Old.StdGen -> Int -> Int -> Int
        go !g !n !acc
            | n >= lim = acc
            | otherwise     =
                    let (a, g') = Old.random g
                    in go g' (n+1) (if a > acc then a else acc)
    print (go g 0 0)

 time $ do
    putStrLn $ "System.Random with our generator"
    let g = pureMT 5
    let go :: PureMT -> Int -> Int -> Int
        go !g !n !acc
            | n >= lim = acc
            | otherwise     =
                    let (a,g') = Old.random g
                    in go g' (n+1) (if a > acc then a else acc)
    print (go g 0 0)

 time $ do
    putStrLn $ "System.Random.Mersenne.Pure"
    let g = pureMT 5
    let go :: PureMT -> Int -> Int -> Int
        go !g !n !acc
            | n >= lim = acc
            | otherwise     =
                    let (a',g') = randomWord64 g
                        a = fromIntegral a'
                    in go g' (n+1) (if a > acc then a else acc)
    print (go g 0 0)

 time $ do
    putStrLn $ "System.Random.Mersenne.Pure generating Double"
    let g = pureMT 5
    let go :: PureMT -> Int -> Double -> Double
        go !g !n !acc
            | n >= lim = acc
            | otherwise     =
                    let (a, g') = randomDouble g
                    in go g' (n+1) (if a > acc then a else acc)
    print (go g 0 0)

 time $ do
    putStrLn $ "System.Random.Mersenne.Pure (unique state)"
    c_init_genrand64_unsafe 5
    let go :: Int -> Int -> IO Int
        go !n !acc
            | n >= lim = return acc
            | otherwise     = do
                    a' <- c_genrand64_int64_unsafe
                    let a = fromIntegral a'
                    go (n+1) (if a > acc then a else acc)
    print =<< go 0 0

 time $ do
    putStrLn $ "System.Random.Mersenne.Unsafe"
    g <- Unsafe.newMTGen (Just 5)

    let go :: Int -> Int -> IO Int
        go !n !acc
            | n >= lim = return acc
            | otherwise     = do
                    a <- Unsafe.random g
                    go (n+1) (if a > acc then a else acc)

    print =<< go 0 0


--    printf "MT is %s times faster generating %s\n" (show $x`div`y) (show (typeOf ty))
--    return ()

