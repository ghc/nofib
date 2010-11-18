
\documentclass{article}
\usepackage{a4wide}
\usepackage{graphics}
\usepackage{graphicx}
\usepackage{color}
\usepackage{alltt}

\usepackage[dvips]{epsfig}
\usepackage{epic}
\usepackage{eepic}


\def\Ipe#1{\def\IPEfile{#1}\input{#1}}

\newenvironment{code}
{\begin{alltt}}
{\end{alltt}}


\title{\sf Finite Automata in Haskell: \\
An Exercise with the Language of Real Numbers}

\author{Jo\~ao Saraiva}

\date{\today}

\begin{document}

\maketitle


\section{Deterministic Finite Automaton}

\begin{figure}[htb!]
{\hrule\bigskip}
\begin{center}
\Ipe{FIGURES/Real.ipe}
\end{center}
\caption{Deterministic Finite Automaton for the Language of the
Real Numbers.}
{\bigskip\hrule}
\end{figure}


\begin{code}
module Language.HaLex.Examples.Real where

import Data.List
import Language.HaLex.RegExp
import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.RegExp2Fa
import Language.HaLex.FaOperations

import Language.HaLex.FaAsDiGraph

import Language.HaLex.Minimize
import Language.HaLex.RegExpAsDiGraph
--import RegExpParser

import Language.HaLex.Fa2RegExp

import qualified Language.HaLex.FaClasses as Fa





sinal'' = (Literal '-') `Or` (Literal '+') `Or` Epsilon

d = Literal 'd'


re_int = sinal'' `Then` d `Then` (Star d)


intdfa = (beautifyDfa . ndfa2dfa . regExp2Ndfa) re_int


d' = (Literal '0') `Or`  (Literal '1') `Or`  (Literal '2') `Or`
     (Literal '3') `Or`  (Literal '4') `Or`  (Literal '5') `Or`
     (Literal '6') `Or`  (Literal '7') `Or`  (Literal '8') `Or`
     (Literal '9')


re_real = sinal'' `Then` (Star d')
                  `Then` ((Literal '.') `Or` Epsilon)
                  `Then` d' `Then` (Star d')

re_real' = sinal'' `Then` (Star d)
                  `Then` ((Literal '.') `Or` Epsilon)
                  `Then` d `Then` (Star d)




cre_real = "('+'|'-')?d*('.')?d+"


realdfa' = (beautifyDfa . ndfa2dfa . regExp2Ndfa) re_real

realdfa'' = (beautifyDfa . minimizeDfa . ndfa2dfa . regExp2Ndfa) re_real'

realdfa''' = (beautifyDfa . stdMinimizeDfa . ndfa2dfa . regExp2Ndfa) re_real'



realdfa'''' = beautifyDfa . minimizeNdfa . regExp2Ndfa $ re_real'

genGraph fa = tographvizIO fa "exp"


realdfa = Dfa ['+','-','.','0','1','2','3','4','5','6','7','8','9']
              ['A','B','C','D','E','F']
              'A'
              ['C','E']
              delta_realdfa

delta_realdfa :: Char -> Char -> Char
delta_realdfa 'A' '+' = 'B'
delta_realdfa 'A' '-' = 'B'
delta_realdfa 'A' '0' = 'C'
delta_realdfa 'A' '1' = 'C'
delta_realdfa 'A' '2' = 'C'
delta_realdfa 'A' '3' = 'C'
delta_realdfa 'A' '4' = 'C'
delta_realdfa 'A' '.' = 'D'
delta_realdfa 'B' '0' = 'C'
delta_realdfa 'B' '1' = 'C'
delta_realdfa 'B' '2' = 'C'
delta_realdfa 'B' '3' = 'C'
delta_realdfa 'B' '4' = 'C'
delta_realdfa 'B' '.' = 'D'
delta_realdfa 'C' '0' = 'C'
delta_realdfa 'C' '1' = 'C'
delta_realdfa 'C' '2' = 'C'
delta_realdfa 'C' '3' = 'C'
delta_realdfa 'C' '4' = 'C'
delta_realdfa 'C' '.' = 'D'
delta_realdfa 'D' '0' = 'E'
delta_realdfa 'D' '1' = 'E'
delta_realdfa 'D' '2' = 'E'
delta_realdfa 'D' '3' = 'E'
delta_realdfa 'D' '4' = 'E'
delta_realdfa 'E' '0' = 'E'
delta_realdfa 'E' '1' = 'E'
delta_realdfa 'E' '2' = 'E'
delta_realdfa 'E' '3' = 'E'
delta_realdfa 'E' '4' = 'E'
delta_realdfa _ _     = 'F'


isreal :: String -> Bool
isreal = Fa.accept realdfa

\end{code}



\section{Non-deterministic Finite Automaton}

\begin{figure}[htb!]
{\hrule\bigskip}
\begin{center}
\Ipe{FIGURES/RealNdfa.ipe}
\end{center}
\caption{Non-deterministic Finite Automaton for the Language of the
Real Numbers.}
{\bigskip\hrule}
\end{figure}


\begin{code}

realndfa = Ndfa ['+','-','.','0','1','2','3','4','5','6','7','8','9']
                ['A','B','C','D','E']
                ['A','C']
                ['C','E']
                deltaNdfa

deltaNdfa 'A' (Just '+') = ['B']
deltaNdfa 'A' (Just '-') = ['B']
deltaNdfa 'A' Nothing    = ['B']

deltaNdfa 'B' (Just '0') = ['C']
deltaNdfa 'B' (Just '1') = ['C']
deltaNdfa 'B' (Just '2') = ['C']
deltaNdfa 'B' (Just '3') = ['C']
deltaNdfa 'B' (Just '4') = ['C']
deltaNdfa 'B' (Just '5') = ['C']
deltaNdfa 'B' (Just '6') = ['C']
deltaNdfa 'B' (Just '7') = ['C']
deltaNdfa 'B' (Just '8') = ['C']
deltaNdfa 'B' (Just '9') = ['C']
--deltaNdfa 'B' Nothing = ['C']
deltaNdfa 'B' (Just '.') = ['D']

deltaNdfa 'C' (Just '.') = ['D']
deltaNdfa 'C' Nothing    = ['B']

deltaNdfa 'D' (Just '0') = ['E']
deltaNdfa 'D' (Just '1') = ['E']
deltaNdfa 'D' (Just '2') = ['E']
deltaNdfa 'D' (Just '3') = ['E']
deltaNdfa 'D' (Just '4') = ['E']
deltaNdfa 'D' (Just '5') = ['E']
deltaNdfa 'D' (Just '6') = ['E']
deltaNdfa 'D' (Just '7') = ['E']
deltaNdfa 'D' (Just '8') = ['E']
deltaNdfa 'D' (Just '9') = ['E']

deltaNdfa 'E' (Just '0') = ['E']
deltaNdfa 'E' (Just '1') = ['E']
deltaNdfa 'E' (Just '2') = ['E']
deltaNdfa 'E' (Just '3') = ['E']
deltaNdfa 'E' (Just '4') = ['E']
deltaNdfa 'E' (Just '5') = ['E']
deltaNdfa 'E' (Just '6') = ['E']
deltaNdfa 'E' (Just '7') = ['E']
deltaNdfa 'E' (Just '8') = ['E']
deltaNdfa 'E' (Just '9') = ['E']

deltaNdfa _ _ = []

-- isrealNdfa :: Fa (Ndfa Char Char) Char => String -> Bool
isrealNdfa = Fa.accept realndfa

\end{code}



Running hugs we have:


\begin{verbatim}
Real> isrealNdfa "-1.1"
True
Real> isrealNdfa "+12."
False
Real> isreal "-1.1"
True
Real> isreal "+12."
False


Real> isrealNdfa "122.122"
True
(1029 reductions, 1818 cells)
\end{verbatim}



\end{document}



