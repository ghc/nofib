module Main(main) where

import	{-fool mkdependHS; ToDo: rm-}
	Parallel

--infixr 0 `_par_`
--infixr 1 `_seq_`

main = print (nfib 30)

nfib :: Int -> Int
nfib n | n <= 1 = 1
     | otherwise = n1 `par` n2 `seq` n1 + n2 + 1
	             where n1 = nfib (n-1) 
			   n2 = nfib (n-2)
