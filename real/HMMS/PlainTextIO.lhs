        \begin{haskell}{PlainTextIO}

> module PlainTextIO( module MaybeStateT, module PlainTextIO ) where

> import MaybeStateT
> import Char(isSpace)--1.3

\end{haskell}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Functions for Reading Data}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        The function \verb~readElements~ reads a list from a character
stream where that stream contains just the plain-text representations
of the elements of the list one after the other but {\em without\/}
the Haskell list notation, i.e., without the starting and ending
square bracks and commas between the elements.  It is important to
note that it is {\em only\/} intended for reading from character
streams that contain {\em only\/} the desired list elements and
nothing else; any characters which can't be parsed to a produce an
element of the desired type will cause a fatal error.

        Most often, \verb~readElements~ will be used to read the
contents of a file.  However, \verb~readElements~ can be used to
extract lists from any character stream---provided that the stream
contains nothing but legal list elements---so another common use might
be to use it to extract lists from the end of each line of a file,
e.g., a pronunciation dictionary.

        The reason we explicitly drop whitespace prior to applying
\verb~reads~ is that this allows us to easily check for the
end-of-file condition if \verb~reads~ can't produce a parse.  If
\verb~reads~ can't produce a parse and we haven't reached the end of
the file, we halt the program and print out a message. That message
includes up to 32 characters from the input stream to help the user
find the problem.
        \begin{haskell}{readElements}

> readElements :: (Read a) => [Char] -> [a]
> readElements cs =
>       let cs' = dropWhile isSpace cs in
>       case reads cs' of
>       [(x,cs'')]   -> x : readElements cs''
>
>       []           -> if null cs'
>                       then []
>                       else error ("readElements: unparsable chars \ 
>                                  \encountered:\n\n" ++ take 32 cs'
>                                  ++ "\n")
>
>       _            -> error ("readElements: ambiguous parse:\n\n"
>                              ++ take 32 cs' ++ "\n")

\end{haskell}
        \fixhaskellspacing\begin{haskell}{readsItem}

> readsItem :: (Read a) => MST [Char] a
> readsItem cs =
>       case reads cs of
>       [(a, cs')] -> Just (a, cs')
>       _          -> Nothing

\end{haskell}

        Some special versions of \verb~readsItem~ for when the type
checker would have difficulty deriving the type of the data to be
read.
        \begin{haskell}{readsInt}

> readsInt :: MST [Char] Int
> readsInt = readsItem

\end{haskell}
        \fixhaskellspacing\begin{haskell}{readsFloat}

> readsFloat :: MST [Char] Float
> readsFloat = readsItem

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Functions for ``Pretty Printing''}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Another way of defining pprintElements might be

        pprintElements f = unlines . map f

where f is a showing function, but then we don't have the
third-argument accumulator string.


        \begin{haskell}{pprintElements}

> pprintElements :: (a -> String -> String) -> [a] -> String -> String

> pprintElements  f  (x:xs)  s =
>       f x ('\n' : pprintElements f xs s)

> pprintElements  _   []     s = s


\end{haskell}
        \fixhaskellspacing\begin{haskell}{pprintAsList}

> pprintAsList :: (a -> String -> String) -> [a] -> String -> String

> pprintAsList  f  xs    s = '[' : pprintAsList' f xs s

> pprintAsList' _  []    s = "]\n" ++ s

> pprintAsList' f (x:xs) s
>       | null xs        = f x (" ]\n" ++ s)
>       | otherwise      = f x (",\n " ++ pprintAsList' f xs s)

\end{haskell}
        \fixhaskellspacing\begin{haskell}{consNewLine}

> consNewline :: String -> String

> consNewline = ('\n' :)

\end{haskell}
