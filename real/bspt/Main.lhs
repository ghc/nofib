\begin{code}
module Main where
	

import Prog (prog)

#ifdef PAR
main = prog 
#else
main = do
    str <- getContents
    putStr (prog str)
#endif
\end{code}
