


\begin{code}


module Language.HaLex.Examples.Robot where

import Language.HaLex.Dfa
import Language.HaLex.FaAsDiGraph
import qualified Language.HaLex.DfaMonad as DfaMonad


\end{code}


\begin{code}


-- ex :: Dfa IO [Char] [Char]
ex = Dfa ["esquerda","direita","largar","pegar"]
         ["C0 sem pepita","C0 com pepita","C1 sem pepita","C1 com pepita"]
         "C0 sem pepita"
         ["C0 sem pepita"]
         delta
  where 
    delta "C0 sem pepita" "direita"  = "C1 sem pepita" 
    delta "C0 sem pepita" _          = "C0 sem pepita" 

    delta "C0 com pepita" "largar"   = "C0 sem pepita" 
    delta "C0 com pepita" "direita"  = "C1 com pepita" 
    delta "C0 com pepita" _          = "C0 com pepita" 

    delta "C1 sem pepita" "esquerda" = "C0 sem pepita" 
    delta "C1 sem pepita" "pegar"    = "C1 com pepita" 
    delta "C1 sem pepita" _          = "C1 sem pepita"

    delta "C1 com pepita" "esquerda" = "C0 com pepita" 
    delta "C1 com pepita" "largar"   = "C1 sem pepita" 
    delta "C1 com pepita" _          = "C1 com pepita"     

moves = ["direita","pegar","esquerda","largar"]
moves2 = ["esquerda","pegar","direita","pegar","esquerda","largar"]
moves3 = ["esquerda","pegar","direita"]
moves4 = moves2 ++ moves

g = dfa2graphviz2file ex "xxx"

\end{code}





\begin{code}

robotM :: DfaMonad.Dfa Maybe [Char] [Char]
robotM = DfaMonad.Dfa ["esquerda","direita","largar","pegar"]
             ["C0 sem pepita","C0 com pepita","C1 sem pepita","C1 com pepita"]
             "C0 sem pepita"
             ["C0 sem pepita"]
             delta
  where 
    delta "C0 sem pepita" "direita"  = return "C1 sem pepita" 
    delta "C0 sem pepita" _          = return "C0 sem pepita" 

    delta "C0 com pepita" "largar"   = return "C0 sem pepita" 
    delta "C0 com pepita" "direita"  = return "C1 com pepita" 
    delta "C0 com pepita" _          = return "C0 com pepita" 

    delta "C1 sem pepita" "esquerda" = return "C0 sem pepita" 
    delta "C1 sem pepita" "pegar"    = return "C1 com pepita" 
    delta "C1 sem pepita" _          = return "C1 sem pepita"

    delta "C1 com pepita" "esquerda" = return "C0 com pepita" 
    delta "C1 com pepita" "largar"   = return "C1 sem pepita" 
    delta "C1 com pepita" _          = return "C1 com pepita"     



acc = DfaMonad.dfaaccept robotM moves4

\end{code}






\begin{code}

robotM2 :: DfaMonad.Dfa IO [Char] [Char]
robotM2 = DfaMonad.Dfa ["esquerda","direita","largar","pegar"]
                       ["C0 sem pepita","C0 com pepita","C1 sem pepita","C1 com pepita"]
                       "C0 sem pepita"
                       ["C0 sem pepita"]
                       delta
  where 
    delta "C0 sem pepita" "direita"  = do putStrLn "Vou para C1 sem pepita"
                                          return "C1 sem pepita" 
    delta "C0 sem pepita" _          = return "C0 sem pepita" 

    delta "C0 com pepita" "largar"   = return "C0 sem pepita" 
    delta "C0 com pepita" "direita"  = return "C1 com pepita" 
    delta "C0 com pepita" _          = return "C0 com pepita" 

    delta "C1 sem pepita" "esquerda" = return "C0 sem pepita" 
    delta "C1 sem pepita" "pegar"    = return "C1 com pepita" 
    delta "C1 sem pepita" _          = return "C1 sem pepita"

    delta "C1 com pepita" "esquerda" = do putStrLn "Vou para C0 com pepita"
                                          return "C0 com pepita" 
    delta "C1 com pepita" "largar"   = return "C1 sem pepita" 
    delta "C1 com pepita" _          = return "C1 com pepita"     



acc2 = DfaMonad.dfaaccept robotM2 moves4


exShow = do d <- DfaMonad.showInDot robotM2 True
            writeFile "xxx" d

\end{code}
