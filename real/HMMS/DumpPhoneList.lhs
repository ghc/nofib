        This utility program dumps a list of the legal phone labels
defined in Chapter~\ref{ch:Phones} to the standard output, one label
per line.
        \begin{haskell}{DumpPhoneList}

> module Main where

> import Phones

> main = appendChan stdout (unlines (map show phone_list)) exit done

\end{haskell}

