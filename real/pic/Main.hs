-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
import	Pic
import  PicType	-- added by partain
import IO(hPutStr,stderr)--1.3

main = do
    hPutStr stderr "Enter the number of particles: "
    s <- getContents
    let (nPart, rest) = (head (reads s)) :: (Int, String)
    putStr (takeWhile ((/=) '\n') s ++ (pic nPart))

{- OLD 1.2:
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
-}
