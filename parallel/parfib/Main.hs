-- -*- haskell -*-
-- Time-stamp: <Fri Mar 30 2001 14:32:21 Stardate: [-30]6407.82 hwloidl>
--
-- Ever popular nfib, now in parallel.
-- Haskell98 version.
-----------------------------------------------------------------------------

module Main(main) where

import System(getArgs)
import	{-fool mkdependHS; ToDo: rm-}
	Parallel

main = do args <- getArgs
          let 
            n = read (args!!0) :: Int  -- input for nfib
            t = read (args!!1) :: Int  -- threshold
            res = parfib n t
          putStrLn ("parfib " ++ (show n) ++ " with threshold " ++ (show t) ++ " = " ++ (show res))

-- parallel version of the code with thresholding
parfib :: Int -> Int -> Int
parfib n t | n <= t = nfib n
           | otherwise = n1 `par` n2 `seq` n1 + n2 + 1
	                 where n1 = parfib (n-1) t
			       n2 = parfib (n-2) t

-- sequential version of the code
nfib :: Int -> Int
nfib 0 = 1
nfib 1 = 1
nfib x = nfib (x-2) + nfib (x-1) + 1
