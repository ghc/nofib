-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
import	GamtebMain

main :: [Response] -> [Request]
main resps =
	[
	Echo True,
	AppendChan stderr "Enter the scale of computation: ",
	ReadChan stdin,
	calcgamteb (resps!!2)
	]

calcgamteb (Str s) =
	AppendChan
	    stdout
	    (takeWhile ((/=) '\n') s ++ (gamteb scale))
	where
	    (scale, rest) = (head (reads s)) :: (Int, String)
