-- -*- haskell -*-
-- Time-stamp: <Thu Mar 15 2001 18:09:18 Stardate: [-30]6333.78 hwloidl>
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
            n = read (args!!0) :: Int
            res = nfib n
          putStrLn ("nfib " ++ (show n) ++ " = " ++ (show res))

nfib :: Int -> Int
nfib n | n <= 1 = 1
       | otherwise = n1 `par` n2 `seq` n1 + n2 + 1
	             where n1 = nfib (n-1) 
			   n2 = nfib (n-2)
