{-# LANGUAGE BangPatterns #-}

import System.Random.Mersenne.Pure64

main = do

    let g = pureMT 7

    let go :: Int -> PureMT -> IO ()
        go 0 !p = return ()
        go n !p  = do
            let (_,q) = randomWord p
            go (n-1) q

    go 10000000 g -- 10s constant space
