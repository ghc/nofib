

        This module provides a number of useful general purpose
functions for processing lists that are not provided in the Haskell
Standard Prelude.
        \begin{haskell}{Lists}

> module Lists( blocks, interleave, interleaveRight,
>               mapfst, mapsnd, mapAccumlfst,
>               foldr1_2op,
>               hamming
>       ) where
> import List(genericSplitAt)--1.3

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{blocks}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        The function \verb~blocks~ breaks a list into a list of
non-overlapping fixed-length sublists (``blocks''), except that the
last block may be shorter than the requested block-size.  Note that
\verb~blocks 0~ applied to a non-empty list returns an infinite list
of empty lists.
        \begin{haskell}{blocks}

> blocks        :: (Integral b) => b -> [a] -> [[a]]
> blocks n []	=  []
> blocks n xs   =  block : blocks n rest
>       where
>	(block, rest) = genericSplitAt n xs

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{interleave, interleaveRight}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        \begin{haskell}{interleave}

> interleave a xs = a : interleaveRight a xs

\end{haskell}
        \fixhaskellspacing\begin{haskell}{interleaveRight}

> interleaveRight a (x:xs) = x : a : interleaveRight a xs
> interleaveRight a   []   = []

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{mapfst, mapsnd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        It will be useful to be able to apply a function pointwise to
the first components in a list of pairs, leaving the second components
untouched.  The function \verb~mapfst~ provides this capability.
        \begin{haskell}{mapfst}

> mapfst                :: (a -> c) -> [(a, b)] -> [(c, b)]
> mapfst  _  []         =  []
> mapfst  f ((a,b):rps) =  (f a, b) : mapfst f rps

\end{haskell}

        The function \verb~mapsnd~ applies a function to the second
components in a list of pairs, leaving the first components untouched:
        \begin{haskell}{mapsnd}

> mapsnd                :: (b -> c) -> [(a, b)] -> [(a, c)]
> mapsnd  _  []         =  []
> mapsnd  f ((a,b):rps) = (a, f b) : mapsnd f rps

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{mapAccumlfst}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        This function is a modified version of the function
\verb~mapAccuml~ provided with \verb~hbc~ in the library module
\verb~ListUtil~.  This version transforms only the first coordinates
of a list of pairs, leaving the second coordinates untouched.
        \begin{haskell}{mapAccumlfst}

> mapAccumlfst :: (s -> a -> (s,b)) ->
>                 s ->
>                 [(a,c)] ->
>                 (s, [(b,c)])

> mapAccumlfst f s []          = (s, [])
> mapAccumlfst f s ((x,d):rps) = (s'', (y,d):rys)
>       where
>       (s', y)    = f s x
>       (s'', rys) = mapAccumlfst f s' rps

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{foldr1\_2op}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        This is a variant of the Standard Prelude function
\verb~foldr1~ that takes two operators.
        \begin{haskell}{foldr1_2op}

> foldr1_2op  op1 _   [x,y]     =  x `op1` y
> foldr1_2op  op1 op2 (x:y:rxs) = (x `op1` y) `op2` foldr1_2op op1 op2 rxs
> foldr1_2op  _   _   []        = error "foldr1_2op []"

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{hamming}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        The function \verb~hamming~ counts the number of positions in
which two equal-length lists differ.  Obviously, generalizations could
be defined for lists of different length, but our only application at
the moment is for lists of equal length.
        \begin{haskell}{hamming}

> hamming :: (Eq a) => [a] -> [a] -> Int

> hamming (x:rxs) (y:rys)
>       | x == y        =     hamming rxs rys
>       | otherwise     = 1 + hamming rxs rys

> hamming []    []      = 0

> hamming []   (_:_)    = error hamming_error

> hamming (_:_) []      = error hamming_error

> hamming_error = "hamming applied to two lists having different lengths"

\end{haskell}

%%%%%%%%%%  End of Lists.lhs  %%%%%%%%%%
