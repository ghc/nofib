
        This module implements the standard ``State Transformer''
monad\index{state transformer
monad}~\cite{Wadler92,Wadler94,KingWadl93}.  Some of the definitions
in this module are taken from the Glasgow distribution (\verb~ghc~)
library file
        \begin{verbatim}
        src/ghc-0.19/ghc/lib/glaExts/PreludeST.lhs
        \end{verbatim}
        and the files
        \begin{quote}
        \verb~demos/Cse/stateMonad.gs ~          and
        \verb~ demos/Ccexamples/ccexamples.gs~
        \end{quote}
        that are part of the Gofer
distribution~\cite{Jones_Gofer2.28,Jones_Gofer2.20}, and are included
here by permission of the authors.  For an elementary introduction to
the monadic style of programming, see~\cite{Partain93}.
        \begin{haskell}{StateT}

> module StateT(
>       ST,
>       returnST, bindST, bindST_, thenST, thenST_,
>       startingFrom, startingWith, maplST, maprST
>       ) where

\end{haskell}


        The type constructor \verb~ST~ represents a {\em state
transformer}\index{state transformer}.  A state transformer represents
a computation that takes an initial state and returns a result paired
with a new value of the state.  The type variable \verb~s~ is the type
of the state and the type variable \verb~a~ is the type of the result.
        \begin{haskell}{ST}

> type ST s a           =  s -> (a, s)

\end{haskell}
        \fixhaskellspacing\begin{haskell}{returnST}

> returnST              :: a -> ST s a
> returnST x s          = (x,s)

\end{haskell}
        \fixhaskellspacing\begin{haskell}{bindST}

> bindST                :: ST s a -> (a -> ST s b) -> ST s b
> bindST m k s          = let  (a, s') = m s  in  k a s'

\end{haskell}
        \fixhaskellspacing\begin{haskell}{bindST_}

> bindST_               :: ST s a -> ST s b -> ST s b
> bindST_ m k s         = let  (_, s') = m s  in  k s'

\end{haskell}
        \fixhaskellspacing\begin{haskell}{thenST}

> thenST                :: ST s a -> (a -> ST s b) -> ST s b
> thenST m k s          = case  m s  of
>                         (a, s') -> k a s'

\end{haskell}
        \fixhaskellspacing\begin{haskell}{thenST_}

> thenST_               :: ST s a -> ST s b -> ST s b
> thenST_ m k s         = case  m s  of
>                         (_, s') -> k s'

\end{haskell}


        The function \verb~startingWith~ applies a state transformer
to an initial state and returns a pair containing the final result and
the final state, while the function \verb~startingFrom~ returns only
the final result, dropping the final state.
        \begin{haskell}{startingWith}

> startingWith          :: ST s a -> s -> (a, s)
> m `startingWith` s0   =  m s0

\end{haskell}
        \fixhaskellspacing\begin{haskell}{startingFrom}

> startingFrom          :: ST s a -> s -> a
> m `startingFrom` s0   =  fst (m s0)

\end{haskell}


        The following two functions were found in the file
\verb~demos/Cse/stateMonad.gs~ that is part of the Gofer distribution.
They are like \verb~map~ but thread a state through the calculation.
Here, \verb~maplST~ threads the state from left to right and
\verb~maprST~ threads the state from right to left.  Thus,
\verb~maplST~ is like the function \verb~mapAccuml~ in the
\verb~ListUtil~ module provided with the Chalmer's Haskell system
\verb~hbc~.  The function \verb~maplST~ is also found in the Glasgow
library under the name \verb~mapST~.  I use `thenST' in both
definitions because that is what Glasgow uses in the definition of
their function \verb~mapST~.
        \begin{haskell}{maplST}

> maplST                :: (a -> ST s b) -> [a] -> ST s [b]
> maplST k []           = returnST []
> maplST k (x:xs)       = k x           `thenST`  \y  ->
>                         maplST k xs   `thenST`  \ys ->
>                         returnST (y:ys)

\end{haskell}
        \fixhaskellspacing\begin{haskell}{maprST}

> maprST                :: (a -> ST s b) -> [a] -> ST s [b]
> maprST k []           = returnST []
> maprST k (x:xs)       = maprST k xs   `thenST`  \ys ->
>                         k x           `thenST`  \y  ->
>                         returnST (y:ys)

\end{haskell}



%%%%%%%%%%  End of StateTransformer.lhs  %%%%%%%%%%
