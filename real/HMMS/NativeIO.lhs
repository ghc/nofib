        \begin{haskell}{NativeIO}

> module NativeIO( module Native,  module MaybeStateT, module NativeIO ) where

> import Native

> import MaybeStateT

\end{haskell}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Functions for Reading Data}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        The function \verb~readVectors~ reads as many lists of values
having length \verb~n~ as it can from the binary stream \verb~bs~.
When it can't read any more, it stops.  If there are left-over bytes,
an error is signaled and execution will stop. Since reading vector
signals is such a common task, we create a specialized version of
\verb~readVectors~ for this case.
        \begin{haskell}{readVectors}

> {-# SPECIALIZE readVectors :: Int -> Bytes -> [[Float]] #-}

> readVectors :: (Native a) => Int -> Bytes -> [[a]]

> readVectors n bs =
>       case (listReadBytes n bs) of
>       Nothing         -> if null bs
>                          then []
>                          else error "there are left-over bytes!"
>       Just (x,bs')   -> x : readVectors n bs'

\end{haskell}

