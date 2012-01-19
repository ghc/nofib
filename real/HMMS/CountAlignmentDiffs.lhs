        This program steps through the new and old alignment files for
all of the training sentences and accumulates a count of how many
frames were aligned differently between two training iterations.  This
difference provides one way of checking for convergence of the
training algorithm.

        The way that we write alignment files, all we need to do is
see if corresponding lines are different; we don't have to actually
parse them into the constituent parts.
        \begin{haskell}{CountAlignmentDiffs}

> module Main where

> import Printf         -- hbc module for C-like printing
> --  import GhcPrintf      -- ghc module for C-like printing

> import Lists

> main = getArgs exit $ \args ->
>       case args of
>       [sent_file] ->  readFile  sent_file  exit  $ \cs ->
>                       let
>                         file_names = lines cs
>                       in
>                         count_diffs 0 0 file_names
>                       
>       _           ->  error usage

> usage = "usage: CountAlignmentDiffs  <sentence-list file>"

\end{haskell}

        The function \verb~hamming~ computes the number of places in
which two equal-length lists differ.  If the lists do not have equal
length, then the program will halt because of how \verb~hamming~ is
implemented.  The function \verb~hamming~ is defined in the module
\verb~Lists~ (Chapter~\ref{ch:Lists}).
        \begin{haskell}{count_diffs}

> count_diffs :: Int -> Int -> StrListCont

> count_diffs  nframes  ndiffs  (fn : rfns) =
>       readFile (fn ++ ".algn") exit           $ \cs1 ->
>       readFile (fn ++ ".algn.old") exit       $ \cs2 ->
>       let
>         a1   = lines cs1
>         a2   = lines cs2
>         n1   = length a1
>         nd   = hamming a1 a2
>         nfs  = nframes + n1
>         nds  = ndiffs  + nd
>       in
>         if (nfs,nds) == (nfs,nds)  -- force evaluation of the
>                                    -- accumulated statistics
>            then  appendChan stdout (show_statistics n1 nd fn)
>                    exit (count_diffs nfs nds rfns)
>            else  error "This can't happen"

> count_diffs nframes ndiffs [] =
>       let
>         p = percentage nframes ndiffs
>       in
>         appendChan stdout (
>         printf "\nNo. frames = %7d\
>                \\nNo. diffs  = %7d\
>                \\nPercentage = %7.1f\n"
>                [UInt nframes, UInt ndiffs, UFloat p]) exit done

\end{haskell}


        \begin{verbatim}

> show_statistics :: Int ->     -- no. frames for this file
>                    Int ->     -- no. different frames for this file
>                    String ->  -- file name
>                    String

> show_statistics nf nd fn =
>       printf "%4d %4d %6.1f  %s\n" [UInt nf, UInt nd,
>               UFloat (percentage nf nd), UString fn]

\end{verbatim}


        \begin{verbatim}

> percentage :: Int -> Int -> Float
> percentage n d = 100.0 * (fromInt d / fromInt n)

\end{verbatim}
