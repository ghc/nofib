\begin{code}
module Main where

import Mandel
import PortablePixmap


main = readChan stdin abort 			 	( \ userInput	   ->
       readNum "Enter min x  = " (lines userInput) 	( \ minx input 	   ->
       readNum "Enter min y  = "  input   		( \ miny input     ->
       readNum "Enter max x  = "  input   		( \ maxx input     ->
       readNum "Enter max y  = "  input   		( \ maxy input     ->
       readNum "Screen width = "  input   		( \ screenX input  ->
       readNum "Screen height= " input   		( \ screenY input  ->
       readNum "Screen depth = "  input   		( \ limit _ 	   ->
       appendChan stdout
		  (show (mandelset minx miny maxx maxy screenX screenY limit))
		  abort done))))))))

readNum::(Num a) => String -> [String] -> (a->[String]->Dialogue) -> Dialogue
readNum prompt inputLines succ
   = appendChan stderr prompt abort
        (case inputLines of
            (x:xs) -> case (reads x) of
	                 [(y,"")] -> succ y xs
			 _	  -> appendChan stderr
				     "Error - retype the number\n" abort
				     (readNum prompt xs succ)
	    _ 	   -> appendChan stderr "Early EOF" abort done)

{-
Enter min x  = -1.5
Enter min y  = -1.0
Enter max x  = 0.5
Enter max y  = 1.0
-}
\end{code}


