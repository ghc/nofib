
        WARNING: Those functions that read native-mode binary files
use the module \verb~Native~ that is provided by the Chalmers Haskell
compiler, ``hbc.''  As of now, standard Haskell does not provide a
capability for reading native-mode binary files.
        \begin{haskell}{SignalIO}

> module SignalIO(
>       ) where

> import Native

\end{haskell}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{readVectors}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

