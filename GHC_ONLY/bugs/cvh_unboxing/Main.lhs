\begin{code}
import Types
import Append

main resps 
 = [AppendChan stdout (show (append_FC_L_L (FC2 a_ a_) []))]
   where a_ = case 'a' of { C# x -> x }
\end{code}
