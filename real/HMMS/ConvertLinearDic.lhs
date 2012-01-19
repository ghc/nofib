
        This program converts a dictionary containing linear
pronunciation models into a dictionary of pronunciation networks
(Chapter~\ref{ch:Pronunciations}).  That is, it takes a dictionary with
entries in the format
        \begin{alltt}

                        A       AX
                        ABLE    EY B EL
                        ABLY    EY B L IY
                        \vdots
        \end{alltt}
        and produces a dictionary with entries in the format
        \begin{alltt}

                        A
                        1  [1]  [1]
                        1       AX      []

                        ABLE
                        3  [1]  [3]
                        1       EY      []
                        2       B       [1]
                        3       EL      [2]

                        ABLY
                        4  [1]  [4]
                        1       EY      []
                        2       B       [1]
                        3       L       [2]
                        4       IY      [3]

                        \vdots
        \end{alltt}


        \begin{haskell}{ConvertLinearDic}

> module Main where

> import Phones

> main = getArgs exit $ \args ->
>       case  args  of
>       [linear_dic_file] -> let
>                              outfile = linear_dic_file ++ ".dgs"
>                            in
>                              readFile linear_dic_file exit $ \cs ->
>                              writeFile outfile (process cs) exit $
>                              appendChan stderr (
>                                "Output written to " ++ outfile ++
>                                "\n") exit done
>
>       _                 -> error usage

> usage = "usage: ConvertLinearDic  <linear dictionary>"

\end{haskell}


        In a ``linear'' dictionary, there is one entry per line.  The
function \verb~readLinearEntry~ reads one line.
        \begin{verbatim}

> type LinearEntry = (String, [Phone])

> readLinearEntry :: String -> LinearEntry
> readLinearEntry l = (w, ps)
>       where (w:pls) = words l
>             ps      = map read pls

> process :: String -> String
> process = concat . map (build_and_show_dg . readLinearEntry) . lines

> build_and_show_dg :: LinearEntry -> String
> build_and_show_dg (w,ps) =
>       let
>         num_nodes  = length ps
>         term_nodes = [num_nodes]
>         node_lines = map show_node_line (
>                         zip3 [1..] ps ([] : [[k] | k <- [1..]]) )
>       in
>         w ++ "\n" ++
>         show num_nodes ++ "  [1]  " ++ show term_nodes ++ "\n" ++
>         unlines node_lines ++ "\n"

> show_node_line (k, p, ps) =
>       show k ++ "\t" ++ show p ++ "\t" ++ show ps

\end{verbatim}

