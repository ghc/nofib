module IO(getFilename,process) where
import Numbers
import Vectors

type InputCont = [String] -> Dialogue

getFilename :: (String -> InputCont) -> InputCont
getFilename success inp = 
	getArgs abort (\args ->
		case  args  of
			"help":_ -> usage
			[filename] ->
				success filename inp
			--	statusFile filename abort (test filename inp)
			[] -> fromInp inp
			_ -> usage
		)
	where
	test filename inp ('t':'r':_) = success filename inp
	test filename inp ('f':'r':_) = success filename inp
	test filename inp _ = 
		appendChan stderr ("Can not read: "++filename++"\n") abort $
		appendChan stderr "Give the filename of an object: " abort $
		fromInp inp
	fromInp [] = done
	fromInp (filename:rest) =
		statusFile filename abort (test filename rest)

usage = appendChan stderr "Usage: hiddenline [filename of object]\n" abort done



getDirection,getit :: (Vector -> InputCont) -> InputCont
getDirection success =
	appendChan stderr ("Give a view direction in the form of: x,y,z\n"++
			   "or 'quit' to stop\n") abort .
	getit success

getit success [] = done
getit success ("quit":ls) = done
getit success (l:ls) =
	case  reads ("vec ["++l++"]") of
		[(v,_)] -> success v ls
		_ -> appendChan stderr "again: " abort (getit success ls)


process :: (Vector -> String -> String) -> String -> InputCont
process f filename =
	getDirection (\viewdir ->
			readFile filename exit .
			printFrom viewdir .
			process f filename
		)
	where printFrom viewdir cont cs =
		appendChan stdout (f viewdir cs) abort cont
