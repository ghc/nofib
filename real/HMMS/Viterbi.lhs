
        The {\em Viterbi algorithm\/} is used to align a hidden Markov
model (HMM) with a sequence of observations.
Chapter~\ref{ch:HmmDigraphs} described how to take a pronunciation
network and turn it into a single HMM.
        \begin{haskell}{Viterbi}

> module Viterbi( module HmmDigraphs, module HmmDensities, align ) where

> import Extrema
> import Lists
> import StateT

> import HmmDigraphs
> import HmmDensities
> import Array--1.3

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Mathematical Description}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        The Viterbi algorithm for estimating the hidden state sequence
given a sequence of observations using logarithmic arithmetic is
explained on page 340 of~\cite{RabiJuan93}.  We present a slightly
modified form here.


        The state estimation problem is also sometimes called the {\em
alignment problem\/} since we are aligning the observation vectors
with a path through the HMM network.


        We begin by defining some notation.  As in \cite{RabiJuan93},
the placement of a tilde over a symbol reminds us that it is a log
value.  For the HMM to be aligned,
        \begin{itemize}

        \item the number of states is denoted by $N$,

        \item the set of starting states is denoted by $\scriptI$,

        \item the log-probability of starting in state $i$
is denoted by $\tilde{\pi}_i$ for all $i \in \scriptI$,

        \item the set of stopping states is denoted by $\scriptT$,

        \item the set of predecessor states of state $j$ is denoted by
$\scriptP_j$,

        \item the log-probability of going from
state $i$ to state $j$ is denoted by $\tilde{a}_{ij}$ for all $i \in
\scriptP_j$, 

        \item the log-density function associated with state $j$ is
denoted by $\tilde{g}_j$.
        \end{itemize}
        Let the list of observation vectors be $[x_1, x_2, \ldots,
x_T]$.  Define $\tilde{p}_t[j]$ to be the {\em path score\/} of the
best path that ends in state $j$ at time $t$, and let $b_t[j]$ be the
{\em backtrace pointer\/} for state $j$ at time $t$, i.e., the index
of the state at time $t-1$ on the best path ending in state $j$ at
time $t$.


        The Viterbi algorithm can be defined in four stages:
initialization, recursion, termination, and backtracking.  The
equations are listed in Table~\ref{tb:viterbi}.  It is easy to see
from the table that the implementation of the algorithm is
straightforward in a conventional imperative language such as C.
        \begin{table}
        \caption[]{The Viterbi Algorithm (Log-probability version)}
        \label{tb:viterbi}
        \begin{itemize}

        \item Initialization. For $i=1,2,\ldots,N$,
        \[
          \tilde{p}_1[i] = \left\{
                \begin{array}{ll}
                \tilde{\pi}_i + \tilde{g}_i(x_1) & i \in \scriptI \\
                -\infty                          & \mbox{otherwise}
                \end{array}
                \right.
        \]

        \item Recursion.  For $t=2,\ldots,T$ and $j=1,2,\ldots,N$,
        \begin{eqnarray*}
           \tilde{p}_t[j] & = & \max_{i \in \scriptP_j}
                                \{ \tilde{p}_{t-1}[i] +
                                   \tilde{a}_{ij} \} \ \ + \ 
                                   \tilde{g}_j(x_t) \\
           b_t[j]         & = & \arg \max_{i \in \scriptP_j}
                                  \{ \tilde{p}_{t-1}[i] + \tilde{a}_{ij} \}
        \end{eqnarray*}

        \item Termination:
        \begin{eqnarray*}
        P^*     & = & \max_{i \in \scriptT}  \{ \tilde{p}_T[i] \} \\
        q^*_T   & = & \arg \max_{i \in \scriptT} \{ \tilde{p}_T[i] \}
        \end{eqnarray*}

        \item Backtrace:
        \[
        q^*_{t-1} = b_t[ q^*_t ],\ \ \ \ t = T, T-1, \ldots, 2
        \]

        \end{itemize}
        \end{table}


        The termination equations could include the exit probabilities
in the calculation, but since determining when an utterance is
finished is somewhat arbitrary, we haven't included them here.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Haskell Implementation}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        The HMM states are indexed by integers.  Hence, the bounds for
an array that has one cell per state has the type \verb~(Int,Int)~.
        \begin{haskell}{HmmStateBounds}

> type HmmStateBounds  = (Int, Int)

\end{haskell}


        Since the HMM states are indexed by integers and the path
scores are floating point values, the collection of path scores can be
stored in an array of the following type:
        \begin{haskell}{PathScores}

> type PathScores = Array Int Float

\end{haskell}


        At each time instant we get a list of backtrace pointers, one
pointer for each HMM state.  The pointer for state $j$ is the integer
index associated with the preceding state on the best scoring path
into state $j$.
        \begin{haskell}{BacktracePtrs}

> type BacktracePtrs = [Int]  -- one element for each HMM state

\end{haskell}


        We keep track of the number of frames that are aligned so that
we can normalize the path score.
        \begin{haskell}{FrameCount}

> type FrameCount = Int

\end{haskell}


        The ``forward'' state in the calculation consists of the array
of path scores and the frame count.  For each observation, we
increment the frame count by one and update the path scores using the
recursion equations of Table~\ref{tb:viterbi} to produce a new value
of the state.
        \begin{haskell}{ForwardState}

> type ForwardState = (PathScores, FrameCount)

\end{haskell}


        The object produced by the forward pass, a pair comprised of a
list of backtrace pointer lists and an array of final path scores, is
called a {\em trellis}.
        \begin{haskell}{Trellis}

> type Trellis = ([BacktracePtrs], ForwardState)

\end{haskell}


        The state scores are initialized to a large negative
number.\footnote{Since the Haskell Standard Prelude defines
\verb~minChar~ and \verb~maxChar~, it seems that it should also define
\verb~minFloat~, \verb~maxFloat~, \verb~minDouble~, \verb~maxDouble~,
etc., for completeness.}
        \begin{haskell}{minFloat}

> minFloat = -1.0e+37 :: Float

\end{haskell}


        The function \verb~forward~ initializes an array of path
scores and then performs the forward pass.  The initial frame
likelihood calculation uses addition because we are using log
probabilities.
        \begin{haskell}{forward}

> forward :: HmmTsA HmmData -> [LogDensityTable] -> Trellis

> forward  (HmmTsA is _ pdg)  (lt:rlts) =
>       maplST (extend_paths (elems pdg) bnds) rlts `startingWith` (nu0, 1)
>       where
>       bnds = bounds pdg
>       nu0  = accumArray (flip const) minFloat bnds
>                 [ (i , let (p,s) = fst (pdg!i)
>                        in  a + lt!p!s)         | (i,a) <- is]

\end{haskell}


        The function \verb~extend_paths~ takes the HMM digraph, the
index bounds for the path score array, a log density table, and
returns a state transformer (Chapter~\ref{ch:StateT}) that maps a
forward state to a list of backtrace pointers and a new forward state.
        \begin{haskell}{extend_paths}

> extend_paths :: ProbDigraphL HmmData ->
>                 HmmStateBounds ->
>                 LogDensityTable ->
>                 ST ForwardState BacktracePtrs

> extend_paths pdg bnds lt (nu,fc) = (btps,(new_nu, fc + 1))
>       where
>       (nu', btps) = unzip [ maximumfst [ (nu!j + a, j) | (j,a) <- ps]
>                             | (_, ps) <- pdg ]
>       new_nu = listArray bnds (zipWith (+) nu'
>                                  [ lt!p!j | ((p,j), _) <- pdg] )

\end{haskell}


        The point of building the trellis is to estimate the sequence
of HMM states.  The estimated sequence is computed by the function
\verb~backtrace~.  The function returns a pair.  The first component
is the path score normalized by the number of frames that were
aligned.  The second component is the optimal state path.
        \begin{haskell}{backtrace}

> backtrace :: HmmTsA HmmData -> Trellis -> (Float, [Int])

> backtrace  (HmmTsA _ ts _)  (bss,(nu,fc))  =  (score',path)
>       where
>       (score, q_star)    = maximumfst [(nu!i, i) | (i,_) <- ts]
>       score'             = score / fromIntegral fc
>       path               = foldr op [q_star] bss
>       psi `op` qs@(q:_)  = (psi!!(q-1)) : qs

\end{haskell}


        The function \verb~decode_state_ids~ is used to dump the
phone-state identities associated with the optimal state path.
        \begin{haskell}{decode_state_ids}

> decode_state_ids :: HmmTsA HmmData -> [Int] -> [HmmData]
> decode_state_ids (HmmTsA _ _ pdg) = map (fst . (pdg!))

\end{haskell}


        The function \verb~align~ combines the forward pass, the
backtrace step, and the phone-state decoding.
        \begin{haskell}{align}

> align :: HmmTsA HmmData -> [LogDensityTable] -> (Float, [HmmData])

> align hmm lts = (score, state_seq)
>       where (score, path) = backtrace hmm (forward hmm lts)
>             state_seq     = decode_state_ids hmm path

\end{haskell}


%%%%%%%%%%  End of Viterbi.lhs  %%%%%%%%%%
