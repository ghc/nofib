        This module provides functions for finding maxima for
situtations not covered by the Standard Prelude functions \verb~max~
and \verb~maximum~.
        \begin{haskell}{Extrema}

> module Extrema( maxfst, maximumfst, maxsnd, maximumsnd ) where

\end{haskell}


        The function \verb~maxfst~ returns that pair that has the
larger first component.  The second component is completely
ignored; if the second argument was in the type class \verb~Ord~ and
if we wanted to examine the second argument in the case of equal first
arguments, then we could just use the standard prelude function
\verb~max~.
        \begin{haskell}{maxfst}

> maxfst :: (Ord a) => (a,b) -> (a,b) -> (a,b)
> maxfst f@(a,b) s@(c,d)
>       | c <= a        = f
>       | otherwise     = s

\end{haskell}
        \fixhaskellspacing\begin{haskell}{maximumfst}

> maximumfst :: (Ord a) => [(a,b)] -> (a,b)
> maximumfst = foldl1 maxfst

\end{haskell}


        The function \verb~maxsnd~ returns that pair that has the
larger second component.  Now the first component is completely
ignored.
        \begin{haskell}{maxsnd}

> maxsnd :: (Ord b) => (a,b) -> (a,b) -> (a,b)
> maxsnd f@(a,b) s@(c,d)
>       | d <= b        = f
>       | otherwise     = s

\end{haskell}
        \fixhaskellspacing\begin{haskell}{maximumsnd}

> maximumsnd :: (Ord b) => [(a,b)] -> (a,b)
> maximumsnd = foldl1 maxsnd

\end{haskell}


%%%%%%%%%%  End of Utilities.lhs  %%%%%%%%%%
