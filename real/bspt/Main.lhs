\begin{code}
module Main where
	

import Prog (prog)

#ifdef PAR
main = prog 
#else
main ~((Str str):_) = [ReadChan stdin, AppendChan stdout (prog str)]
#endif
\end{code}
