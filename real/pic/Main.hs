-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
import	Pic
import  PicType	-- added by partain

main :: [Response] -> [Request]
main resps =
	[
	Echo True,
	AppendChan stderr "Enter the number of particles: ",
	ReadChan stdin,
	calcpic (resps!!2)
	]

calcpic (Str s) =
	AppendChan
	    stdout
	    (takeWhile ((/=) '\n') s ++ (pic nPart))
	where
	    (nPart, rest) = (head (reads s)) :: (Int, String)
