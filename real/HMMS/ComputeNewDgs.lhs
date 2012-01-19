        \begin{haskell}{ComputeNewDgs}

> module Main where

> import ListUtil( group, groupEq )
> import Printf

> import Lists( mapsnd )
> import PlainTextIO

> import Phones
> import HmmDigraphs
> import Alignments

> main = getArgs exit $ \args ->
>       case args of
>       [hmm_file,
>        utts_file] -> readFile  hmm_file   exit  $ \cs1 ->
>                      readFile  utts_file  exit  $ \cs2 ->
>                      let
>                        initial_count_tables = 
>                          amap transform_Hmm_to_Cnt (
>                            build_hmm_array (
>                              readHmms cs1))
>
>                        file_names = lines cs2
>                      in
>                        collect_counts initial_count_tables file_names
>                       
>       _          -> error (" Bad command line\n" ++ usage)

> usage = "usage:  ComputeNewDgs  <hmm network file>  <utts list file>"

\end{haskell}


        For each HMM, we need a table of cumulative counts.  To
initialize these tables, we need to know the topology of each HMM.
There must be an accumulator for each arc in the HMM.  Notice that the
count structure has the same ``shape'' as the \verb~HmmTsL~ data
structure (Chapter~\ref{ch:HmmDigraphs}).  What we do is read in the
hmms that were used to get this alignment data (or, we could edit that
file to create whatever topologies we would like them to have) and
just transform those structures into count tables.
        \begin{haskell}{StateCounts}

> data StateCounts =
>       SC  Int  [(Int,Int)]  [(Int,Int)]  [((Int,Int), [(Int,Int)])]
>       deriving Eq

\end{haskell}
        \fixhaskellspacing\begin{haskell}{CountTable}

> type CountTable = Array Phone StateCounts

\end{haskell}


        The function \verb~collect_counts~ takes a cummulative count
table and a list of file names and updates the cummulative count file
after processing each file.  When the file name list is exhausted, the
function prints out the final results.
        \begin{haskell}{collect_counts}

> collect_counts  cnt_table  [] =
>       let
>         probs = amap counts_to_probs cnt_table
>       in
>         appendChan stderr 
>           "New digraphs written to new_hmms.dgs\n" exit $
>         writeFile "new_hmms.dgs" (showsHmmTsLs probs "") exit done

> collect_counts  cnt_table  (fn:rfns) =
>       appendChan stderr (fn ++ "\n") exit    $
>       readFile (fn ++ ".algn") exit          $ \cs ->
>       let
>         states        = strip_off_frame_number (readAlignment cs)
>         segments      = groupEq (\ (a,_) (b,_) -> a==b) states
>         counts        = map (get_segment_counts cnt_table) segments
>         new_cnt_table = accum add_counts cnt_table counts
>       in
>         if new_cnt_table == new_cnt_table  -- idiom to force 
>                                            -- evaluation of the
>                                            -- new count table
>            then  collect_counts  new_cnt_table  rfns
>            else  error "This can't happen"

\end{haskell}


        The function \verb~get_segment_counts~ collects the state
occupancy and state transition counts for a {\em segment}, which is
defined as a run in a single HMM.
        \begin{haskell}{get_segment_counts}

> get_segment_counts  count_table  segment_path  =
>       let
>         (ps, js)     = unzip segment_path
>         hmm          = head ps
>         start_state  = head js
>         end_state    = last js
>         SC n _ _ _   = count_table!hmm
>         state_counts = accumArray (+) 0  (1,n) (map (:= 1) js)
>         trn_counts   = accumArray (+) 0 ((1,1),(n,n))
>                               (map (:= 1) (zip (tail js) js))
>       in
>         hmm := (start_state, end_state, state_counts, trn_counts)

\end{haskell}


        The function \verb~add_counts~ takes the state occupancy and
state transition counts for a segment and adds them to the cummulative
count table for the corresponding hmm.
        \begin{haskell}{add_counts}

> add_counts (SC n is ts dg) (start_state, end_state,
>                               state_counts, trn_counts)
>       = SC n 
>              [if k == start_state
>                  then (k, n+1)
>                  else (k, n)  | (k,n) <- is]
>
>              [if k == end_state
>                  then (k, n+1)
>                  else (k, n)  | (k,n) <- ts]
>
>              [ ((i, n + state_counts!i), [(k, m + trn_counts!(i,k)) 
>                                          | (k,m) <- pcs] )
>                | ((i, n), pcs) <- dg ]

\end{haskell}


        The function \verb~counts_to_probs~ takes a cummulative count
table and converts it to probabilities.
        \begin{haskell}{counts_to_probs}

> counts_to_probs (SC n is ts dg) =
>       if num_starts == num_stops
>          then  HmmTsL n is' ts' dg'
>          else  error ("Number of starts /= number of stops.\n\
>                        \starts: " ++ shows is "\n" ++
>                        "stops : " ++ shows ts "\n")
>       where
>
>       num_starts       = sum (snd (unzip is))
>       num_stops        = sum (snd (unzip ts))
>
>       (ids_and_cnts, pss)  = unzip dg
>       (ids, cnts)          = unzip  ids_and_cnts
>       fcounts              = map fromInt cnts
>
>       is' = let divisor = fromInt num_starts
>             in mapsnd ((/divisor) . fromInt) is
>
>       ts' = [(k, fromInt m / fcounts!!(k-1)) | (k, m) <- ts]
>
>       dg' = zip ids [ [ (k, fromInt m / fcounts!!(k-1))
>                          | (k,m) <- ps ]
>                       | ps <- pss]

\end{haskell}


\section {Output Functions}

        The following functions are for writing the HMM topologies.
The functions for reading them were defined in the module
\verb~HmmDigraphs~ (Chapter~\ref{ch:HmmDigraphs}).
        \begin{haskell}{showsHmmTsLs}

> showsHmmTsLs = pprintElements showsHmmTsL . assocs

\end{haskell}
        \fixhaskellspacing\begin{haskell}{showsHmmTsL}

> showsHmmTsL  (phn := (HmmTsL n is es dg)) =
>       shows phn .
>       (" :=\nHmmTsL " ++) .
>       shows n .
>       consNewline .
>       shows is .
>       consNewline .
>       shows es .
>       consNewline .
>       pprintAsList shows dg

\end{haskell}


        \begin{verbatim}

> transform_Hmm_to_Cnt :: (HmmTsL Int) -> StateCounts
> transform_Hmm_to_Cnt (HmmTsL n is es dg) =
>       SC n 
>       (mapsnd (const 0) is)
>       (mapsnd (const 0) es)
>       (map (\(n,ps) -> ((n,0), mapsnd (const 0) ps)) dg)

\end{verbatim}

%%%%%%%%%%  ComputeNewDgs.lhs  %%%%%%%%%%
