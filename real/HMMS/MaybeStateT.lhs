        This module implements the {\em maybe state transformer},
which is the state transformer monad (Chapter~\ref{ch:StateT}) coupled
with the ``Maybe'' monad, where the ``Maybe'' type is wrapped around
the pair containing the result and the new state.  This approach to
combining the State Transformer monad with another monad was suggested
in~\cite{KingWadl93}.  The ``Maybe'' type is implemented in the
\verb~hbc~ library module \verb~Maybe~.  Like the state transformer,
the maybe state transformer represents a calculation that takes an
initial state and returns a result paired with a new value of the
state; however, the maybe state transformer is used when it is
possible for the calculation to fail.  We mostly use this monad to
structure functions that read data files.
        \begin{haskell}{MaybeStateT}

> module MaybeStateT(
>#ifndef __GLASGOW_HASKELL__
>       module Maybe,
>#endif
>       MST(..),
>       returnMST, bindMST, thenMST
>       ) where

>#ifndef __GLASGOW_HASKELL__
> import Maybe
>#endif

\end{haskell}
        \fixhaskellspacing\begin{haskell}{MST}

> type MST s a  =  s -> Maybe (a, s)

\end{haskell}
        \fixhaskellspacing\begin{haskell}{returnMST}

> returnMST     :: a -> MST s a
> returnMST x   = \s -> Just (x, s)

\end{haskell}
        \fixhaskellspacing\begin{haskell}{bindMST}

> bindMST       :: MST s a -> (a -> MST s b) -> MST s b
> bindMST m k s =  m s `thenM` \(x, s') -> k x s'
>   where
>     m `thenM` k = case m of
>		  Nothing -> Nothing
>		  Just a  -> k a

\end{haskell}
        \fixhaskellspacing\begin{haskell}{thenMST}

> thenMST       :: MST s a -> (a -> MST s b) -> MST s b
> thenMST m k s =  case  m s of
>                  Nothing      -> Nothing
>                  Just (x, s') -> k x s'

\end{haskell}
