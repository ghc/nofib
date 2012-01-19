

\documentclass{article}
\usepackage{alltt}


\newenvironment{code}
{\begin{alltt}}
{\end{alltt}}


\title{Deterministic Finite Automaton with Effects}

\author{Jo\~ao Saraiva}

\date{\today}

\begin{document}

\maketitle




\begin{code}
module Language.HaLex.DfaMonad where

import Data.Char
import Data.List
import Language.HaLex.Ndfa
import Control.Monad
import Control.Monad.State

import System.IO.Unsafe
import Data.IORef

--
-- Generic Functions to Implement a Recognizer based on Finite State Machines
-- (Deterministic Finite Automata)
--

data Dfa m st sy  = Dfa [sy]                -- Vocabulary
                        [st]                -- Finite set of states
                        st                  -- The start state
                        [st]                -- The set of final states
                        (st -> sy -> m st)  -- Monadic Transition Function
\end{code}


The function \texttt{dfawalk} recurses over a list. As a consequence,
we can re-write that function using the pre.defined Monadic fold
function \texttt{foldM}..

\begin{code}
dfawalk :: Monad m => (st -> sy -> m st) -> st -> [sy] -> m st
dfawalk delta s = foldM delta s
\end{code}


\begin{code}
dfaaccept' :: (Monad m , Eq st) => Dfa m st sy -> [sy] -> m Bool
dfaaccept' (Dfa _ _ s z delta) sent =
  do st <- foldM delta s sent
     return (st `elem` z)

dfaaccept :: (Monad m, Eq st) => Dfa m st sy -> [sy] -> m Bool
dfaaccept (Dfa _ _ s z delta) sent =
  do { st <- dfawalk delta s sent
     ; return (st `elem` z)
     }
\end{code}



\begin{code}

runDfa :: Eq st => Dfa (State s) st sy -> [sy] -> s -> (Bool,s)
runDfa dfa str initSt = runState (dfaaccept dfa str) initSt

\end{code}


\begin{code}

showDfa :: (Monad m, Show st, Show sy) => Dfa m st sy -> m String
showDfa (Dfa v q s z delta) =
  do let a = showString ("dfa = Dfa v q s z delta") .
             showString ("\n  where \n\t v = ") .
             showList v .
             showString ("\n\t q = ") .
             showList q .
             showString ("\n\t s = ") .
             shows s .
             showString ("\n\t z = ") .
             showList z .
             showString ("\n\t -- delta :: st -> sy -> st \n")
     b <- showDfaDelta q v delta
     return ((a . b) "" )

showDfaDelta :: (Monad m, Show st, Show sy) => [st] -> [sy] -> (st -> sy -> m st) ->
                                               m ([Char] -> [Char])
showDfaDelta q v d =
  do let l     = [(a,b) | a <- q , b <- v]
     let (m,n) = unzip l
     q'        <- mapM (uncurry d) l
     let f     = zipWith3 showF m n q'
     return (foldr (.) (showChar '\n') f)
  where
    showF st sy st' = showString("\t delta ") .
                      shows st .
                      showChar(' ') .
                      shows sy .
                      showString(" = return ") .
                      shows st' .
                      showChar('\n')
\end{code}


\begin{code}

showInDot :: (Monad m , Eq st , Show st , Show sy)
          => Dfa m  st sy             -- The Monadic Deterministic Finite Automaton
          -> Bool                     -- Result's Type: True -> Show Dead States
                                      --                 False -> Don't Show Dead States
          -> m [Char]                 -- The Dot string
showInDot dfa@(Dfa v q s z delta) t =
   do dsts <- deadstates dfa
      let a = showString ("digraph fa {\n") .
              showString ("rankdir = LR ;\n") .
              showString ("orientation=land;\n") .
              (showInitialState s) . showChar '\n' .
              showString (showElemsListPerLine (showFinalStates' z)) .
              showString ("node [shape=circle , color=black];\n")
      b <- showArrows dfa dsts t
      return ((a (concat b)) ++ "}\n")


showElemsListPerLine :: [String] -> String
showElemsListPerLine []    = ""
showElemsListPerLine (h:t) = ((showString h) "\n") ++ (showElemsListPerLine t)

showInitialState s = showString("\"") . shows s .  showChar('\"') .
                     showString (" [shape=circle , color=green];\n")

-- showFinalStates' :: Show a => [a] -> [String]
showFinalStates' z = [ "\"" ++ (show f) ++ "\""
                       ++ " [shape=doublecircle , color=red];" | f <- z ]

showArrows :: (Monad m, Eq st , Show st, Show sy)
           => Dfa m st sy -> [st] -> Bool -> m [[Char]]
showArrows (Dfa v qs s z delta) dsts t  =
   do let qs' = if t then qs else qs <-> dsts
      xpto [ mapM (buildLine q delta dsts t) v | q <- qs'  ]

buildLine :: (Monad m , Eq st , Show st, Show sy)
          => st -> (st -> sy -> m st) -> [st] -> Bool -> sy -> m [Char]
buildLine q delta dsts t v =
  do let a = showChar('\"') . shows q . showChar('\"') . showString (" -> \"")
     q' <-  delta q v
     let line = if ( (not t) && q' `elem` dsts ) then ""
                else ((a . shows q' . showString ("\" [label = \"") . shows v .
                      showString("\"];\n") ) "")
     return line


(<->) :: Eq a => [a] -> [a] -> [a]
l1 <-> l2 = [ x | x <- l1 , not (x `elem` l2) ]



xpto :: Monad m => [m [a]] -> m [a]
xpto [] = return []
xpto (x:xs) = do rx <- x
                 rxs <- xpto xs
                 return (rx++rxs)
\end{code}



\begin{code}

deadstates :: (Monad m , Eq st) => Dfa m st sy -> m [st]
deadstates (Dfa v qs s z d) = deadstates' qs v d

deadstates' :: (Monad m, Eq st) => [st] -> [sy] -> (st -> sy -> m st) -> m [st]
deadstates' [] _ _ = return []
deadstates' (q:qs) v delta =
  do rq <- isStDead q (mapM (delta q) v)
     rqs <- deadstates' qs v delta
     return (if rq then q : rqs else rqs)

--
-- A state a is dead whenever a final state can not be reached from a.
-- (this function checks for synv states only: states that for every symbol
--

isStDead  :: (Monad m, Eq st) => st -> m [st] -> m Bool
isStDead = isSyncState

--
-- We've implemented a simpler function only: isSyncState
-- A non initial, non final state A is said to be a sync state
-- iff for every vocabulary symbol the automaton moves to A.
--

isSyncState :: (Monad m, Eq st) => st -> m [st] -> m Bool
isSyncState st msts = do sts <- msts
                         return (and (isin' st sts))
  where isin' e []     = [True]
        isin' e (x:xs) = (e==x) : isin' e xs

\end{code}



\section{Maybe Monad based Automata}

\begin{code}

robot :: Dfa Maybe [Char] [Char]
robot = Dfa ["esquerda","direita","largar","pegar"]
            ["C0 sem pepita","C0 com pepita","C1 sem pepita","C1 com pepita"]
            "C0 sem pepita"
            ["C0 sem pepita"]
            delta
  where
    delta "C0 sem pepita" "direita"  = -- do putStrLn "Vou para C1 sem pepita"
                                          return "C1 sem pepita"
    delta "C0 sem pepita" _          = return "C0 sem pepita"

    delta "C0 com pepita" "largar"   = return "C0 sem pepita"
    delta "C0 com pepita" "direita"  = return "C1 com pepita"
    delta "C0 com pepita" _          = return "C0 com pepita"

    delta "C1 sem pepita" "esquerda" = return "C0 sem pepita"
    delta "C1 sem pepita" "pegar"    = return "C1 com pepita"
    delta "C1 sem pepita" _          = return "C1 sem pepita"

    delta "C1 com pepita" "esquerda" = -- do putStrLn "Vou para C0 com pepita"
                                          return "C0 com pepita"
    delta "C1 com pepita" "largar"   = return "C1 sem pepita"
    delta "C1 com pepita" _          = return "C1 com pepita"


moves = ["direita","pegar","esquerda","largar"]
moves2 = ["esquerda","pegar","direita","pegar","esquerda","largar"]
moves3 = ["esquerda","pegar","direita"]
moves4 = moves2 ++ moves ++ moves3

acc = dfaaccept robot moves4

{-
exShow = do d <- showInDot robot True
            writeFile "xxx" d
-}

\end{code}


\begin{verbatim}
Dfa> dfaaccept ex "aba"
Just True
\end{verbatim}


\section{IO Monad based Automata}


\begin{code}
varGlob = unsafePerformIO $ newIORef ""

ex2 :: Dfa IO Char Char
ex2 = Dfa ['a','b']
         ['A','B','C','D']
         'A'
         ['C']
         delta
  where
    delta 'A' 'a' = do  v <- readIORef varGlob
                        writeIORef varGlob ('A':v)
                        putStrLn ('A':v)
                        return 'A'
    delta 'A' 'b' = do  putStrLn "b"
                        return 'B'
    delta 'B' 'a' = return 'C'
    delta _ _ = return 'D'
\end{code}

\begin{verbatim}
Dfa>  dfaaccept ex2 "aba"
A
b
True
\end{verbatim}


\section{State Monad Based Automata}


The State Monad

\begin{code}


{-
data State s v = State (s -> (v,s))

instance Show (State s v) where
  show = showState

apply :: State s v -> s -> (v,s)
apply (State f) s = f s


instance Monad (State s) where
  return x        = State f
                      where f s = (x,s)
  p >>= q         = State f
                      where f s = let
                                     (x,s') = apply p s
                                  in apply (q x) s'

runState :: State s v -> s -> (v,s)
runState (State f) s0 = let (v,s) = f s0 in (v,s)
-}

\end{code}


One Example

\begin{code}

-- er: a* b a
-- Conta o numero de ocorrência dos caracter a

ex3 :: Dfa (State Integer) Char Char
ex3 = Dfa ['a','b']
         ['A','B','C']
         'A'
         ['C']
         delta
  where
    delta 'A' 'a' = do countA
		       return 'A'
    delta 'A' 'b' = return 'B'
    delta 'B' 'a' = do countA
                       return 'C'

    countA = State f
      where f s = ((),s+1)

runAccept dfa str = runState (dfaaccept dfa str) 0


\end{code}




\begin{code}

-- er: a* b a
-- Accumulates in the State the occurrences of caracter a

ex4 :: Dfa (State [Char]) Char Char
ex4 = Dfa ['a','b']
          ['A','B','C']
          'A'
          ['C']
          delta
  where
    delta 'A' 'a' = do modify (\ s -> 'a':s)
		       return 'A'
    delta 'A' 'b' = return 'B'
    delta 'B' 'a' = do modify (\ s -> 'a':s)
                       return 'C'


runAccept_ex4 dfa str = runState (dfaaccept dfa str) ""


\end{code}



\subsection{Computes Tracing Information}

It computes the visited states (nodes).

\begin{code}

ex5 :: Dfa (State [Char]) Char Char
ex5 = Dfa ['a','b']
          ['A','B','C']
          'A'
          ['C']
          delta
  where
    delta 'A' 'a' = do { accum 'A' ; return 'A' }
    delta 'A' 'b' = do { accum 'B' ; return 'B' }
    delta 'B' 'a' = do { accum 'C' ; return 'C' }

    accum x = modify (\ s -> x:s)

runAccept_ex5 str = runState (dfaaccept ex5 str) ""
\end{code}





\subsection{The language of Integer Numbers}



\begin{code}

-- er: ('+'|'-')?d+
-- Accumulates in the State the occurrences of characters '-' and 'd'

ex_int :: Dfa (State [Char]) Integer Char
ex_int = Dfa ['+','-','d']
             [1,2,3]
             1
             [3]
             delta
  where
    delta 1 '+' = return 2
    delta 1 '-' = do accumM
                     return 2
    delta 1 'd' = do accumD
                     return 3
    delta 2 'd' = do accumD
                     return 3
    delta 3 'd' = do accumD
                     return 3

    accumM = State f
      where f s = (s,'-':s)

    accumD = State f
      where f s = (s,'d':s)


runAccept_int str = runState (dfaaccept ex_int str) ""
\end{code}




The language of Integer Numbers Follow by " "



\begin{code}

-- er: ('+'|'-')?d+
-- Accumulates in the State the occurrences of characters '-' and 'd'

ex6 :: Dfa (State ([Char],Int)) Integer Char
ex6 = Dfa ['+','-','1','2',' ']
          [1,2,3,4,5]
          1
          [4]
          delta
  where
    delta 1 '+' = return 2
    delta 1 '-' = do { accum '-' ; return 2 }
    delta 1 '1' = do accum '1'
                     return 3
    delta 1 '2' = do accum '2'
                     return 3
    delta 2 '1' = do accum '1'
                     return 3
    delta 2 '2' = do accum '2'
                     return 3
    delta 3 '1' = do accum '1'
                     return 3
    delta 3 '2' = do accum '2'
                     return 3
    delta 3 ' ' = do convert
                     return 4

    delta _ _ = return 5                      -- To complete the DFA

    accum x = State f
      where f s = ((),((fst s)++[x],snd s))

    convert = State f
      where f s = ((),(fst s,(read (fst s))::Int))


runAccept_ex6 str = runState (dfaaccept ex6 str) ("",0::Int)


\end{code}

\begin{verbatim}
Dfa> runAccept_ex6 "-12 "
(True,("-12",-12))
\end{verbatim}

\section{A Text Editor Interpreter}


\begin{code}
type Instr = [Code]

data Code = Open   String
          | Locate Int
          | Insert String
          | Delete [Int]
          | Save
          | End
          deriving Show
\end{code}



\begin{code}
te :: Dfa (State ([Char],[Code])) Integer Char
te = Dfa ['A','x','1','0',' ']
         [1,2,3,4,9,10,20]
         1
         [3,4]
         delta
  where
    delta 1 'A' = do init
                     return 2
    delta 2 x | isLower x  = do accum x
                                return 3
              | otherwise  = return 20
    delta 3 'R' = do open
                     return 4
    delta 3 x | isLower x || isDigit x  = do accum x
                                             return 3
              | otherwise = return 20
    delta 4 'A' = do init
                     return 2

    delta 4 'P' = do init
                     return 9
    delta 9 x | isDigit x = do accum x
                               return 10
    delta 10 x | isDigit x = do accum x
                                return 10
    delta 10 'R' = do locate
                      return 4

    delta _ _ = return 20


    accum x = State f
      where f s = ((),((fst s)++[x],snd s))

    init = State f
      where f s = ((),("",snd s))

    open = State f
      where f s = ((),(fst s,(snd s) ++ [Open (fst s)]))

    locate = State f
      where f s = ((),(fst s,(snd s) ++ [Locate (read (fst s))]))


runAccept_te str = runState (dfaaccept te str) ("",[])

\end{code}


\begin{verbatim}
Dfa> runAccept_te "Aficheiro10RP12R"
(True,("12",[Open "ficheiro10",Locate 12]))
\end{verbatim}



\section{Protocol}

"000((0|1)(0|1)(0|1)(001(0|1)(0|1)(0|1))*)111"

\begin{code}
pr :: Dfa (State ([Char],[Int])) Integer Char
pr = Dfa ['1','0']
         [1,2,3,4,5,6,7,8,9,10,11,12,13]
         1
         [12]
         delta
  where
    delta 1  '0' = return 2
    delta 2  '0' = return 3
    delta 3  '0' = return 4
    delta 4  '0' = do { accum '0' ; return 5 }
    delta 4  '1' = do { accum '1' ; return 5 }
    delta 5  '0' = do { accum '0' ; return 6 }
    delta 5  '1' = do { accum '1' ; return 6 }
    delta 6  '0' = do { accum '0' ; accumList ; return 7 }
    delta 6  '1' = do { accum '1' ; accumList ; return 7 }
    delta 7  '0' = return 8
    delta 7  '1' = return 9
    delta 8  '0' = return 10
    delta 9  '1' = return 11
    delta 10 '1' = do { init ; return 4 }
    delta 11 '1' = do { init ; return 12 }

    delta _ _ = return 13


    accum x = modify (\ s -> ((fst s)++[x],snd s))
    init        = modify (\ s -> ("",snd s))
    accumList   = modify (\ s -> (fst s,snd s ++ [converte (fst s)]))


converte :: [Char] -> Int
converte []       = 0
converte ('0':xs) = converte xs
converte ('1':xs) = expo 2 (length xs) + converte xs

expo v e | e > 0     = v * (expo v (e-1))
         | otherwise = 1

runAccept_pr :: [Char] -> (Bool,([Char],[Int]))
runAccept_pr str = runState (dfaaccept pr str) ("",[])

-- runAccept_gv = writeFile "xx.dot" (fst (runState (showDfaDiGraph  pr) ("",[])))


\end{code}

\end{document}
