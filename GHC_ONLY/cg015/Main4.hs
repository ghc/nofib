import GlasgowIOMonad
import GlasgowIO

main =  if foo == (16::Int) then
	    finish 65#	-- 'A'
	else
	    finish 66#	-- 'B'
     where
       foo = twice twice twice inc 0

       twice f x = f (f x)

       inc :: Int -> Int
       inc x = x+1

finish :: IntPrim -> IO ()
finish n = ccall putchar n `thenIOPrim_` returnIO ()
