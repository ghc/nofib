import GlasgowIOMonad
import GlasgowIO

data CList = CNil | CCons Int CList

mk :: Int -> CList
mk n = if (n == 0) then
	 CNil
       else
       	 CCons 1 (mk (n - 1))

clen :: CList -> Int
clen CNil = 0
clen (CCons _ cl) = 1 + (clen cl)

main = case (clen list4) of
	len4 ->
	  case (len4 `plusInt` len4) of
	    8 -> finish 65#	-- 'A'
	    _ -> finish 66#	-- 'B'
      where
      list4	= mk 4

finish :: IntPrim -> IO ()
finish n = ccall putchar n `thenIOPrim_` returnIO ()
