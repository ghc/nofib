-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
import	GamtebMain
import IO--1.3

main = do
    hPutStr stderr "Enter the scale of computation: "
    s <- getContents
    let (scale, rest) = (head (reads s)) :: (Int, String)
    putStr (takeWhile ((/=) '\n') s ++ (gamteb scale))

{- OLD: 1.2
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
-}
