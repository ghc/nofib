import GlasgowIOMonad
import GlasgowIO

main =  if foo == (1::Int) then
	    finish 65#	-- 'A'
	else
	    finish 66#	-- 'B'
     where
       foo = f (f 3)

       f = if ((3::Int) > (4::Int)) then inc else dec

       inc, dec :: Int -> Int
       inc x = x+1
       dec x = x-1

finish :: IntPrim -> IO ()
finish n = ccall putchar n `thenIOPrim_` returnIO ()
