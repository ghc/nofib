        This program aligns a collection of transcribed speech files
with their hidden Markov models.  For each speech file, a plain-text
alignment file is produced.
        \begin{haskell}{BatchAlign}

> module Main where

\end{haskell}


        The module \verb~Printf~ is a library module provided with the
Chalmers and Glasgow Haskell compilers.  It allows C-like printing of
integers, floating point numbers, and strings.
        \begin{verbatim}

> import Printf         -- needed if you want to use hbc; comment
>                       -- out this import if you use ghc v0.19

> -- import GhcPrintf      -- needed if you want to use ghc v0.19;
>                       -- comment this import out if you use hbc


\end{verbatim}


        The following modules are from a general library and are
described in later chapters in Part~\ref{part:library}.
        \begin{verbatim}

> import NativeIO
> import PlainTextIO

\end{verbatim}


        The following modules are specifically for training HMMs.
They were documented in earlier chapters (Part~\ref{part:modules}).
        \begin{verbatim}

> import Phones
> import Pronunciations
> import HmmDigraphs
> import HmmDensities
> import Viterbi
> import HmmConstants
>#if __HASKELL1__ >= 3
> import System	( getArgs )
> import Array
> import IO
> type Assoc a b = (a,b)
> (=:) a b = (a,b)
>#endif

#if __HASKELL1__ < 5
#define amap map
#else
#define amap fmap
#endif


\end{verbatim}


        In the main expression, the function \verb~build_tmt~
(Section~\ref{sc:tmt}) builds the tied-mixture table and then applies
a continuation.
        \begin{verbatim}

>#if __HASKELL1__ < 3
> main = getArgs exit $ \args ->
>       case args of
>       [gms_dir,   -- Gaussian mixtures directory
>        dmap_file, -- density map file
>        dgs_file,
>        utts_file] -> readFile  dmap_file  exit $ \cs0 ->
>                      readFile  dgs_file   exit $ \cs1 ->
>                      readFile  utts_file  exit $ \cs2 ->
>                      let
>
>                        density_map = concat (
>                                        map restructure (
>                                          readElements cs0))
>
>                        hmm_dgs = get_log_probs (
>                                    build_hmm_array (
>                                      readHmms cs1))
>
>                        file_names = lines cs2
>
>                      in
>                        build_tmt  gms_dir  density_map  []  $
>                          \hmm_tms -> align_each_file hmm_tms hmm_dgs
>                                        0 0.0 file_names
>                       
>       _           -> error usage
>#else
> main = getArgs >>=  \args ->
>       case args of
>       [gms_dir,   -- Gaussian mixtures directory
>        dmap_file, -- density map file
>        dgs_file,
>        utts_file] -> readFile  dmap_file  >>= \cs0 ->
>                      readFile  dgs_file   >>= \cs1 ->
>                      readFile  utts_file  >>= \cs2 ->
>                      let
>
>                        density_map = concat (
>                                        map restructure (
>                                          readElements cs0))
>
>                        hmm_dgs = get_log_probs (
>                                    build_hmm_array (
>                                      readHmms cs1))
>
>                        file_names = lines cs2
>
>                      in
>                        build_tmt  gms_dir  density_map  []  >>=
>                          \hmm_tms -> align_each_file hmm_tms hmm_dgs
>                                        0 0.0 file_names
>                       
>       _           -> error usage
>#endif


> usage = "usage: BatchAlign  <gms dir>  <density map file> <dgs file>  <utt list file>"
> -- partain: got rid of string gap because of doing -cpp

\end{verbatim}


%----------------------------------------------------------------------
\section {The Density Map}
%----------------------------------------------------------------------

        The association list that tells us which HMM states have their
own mixtures and which are tied to other HMM states is stored in a
{\em density map file}.  The name of this file is the second argument
on this program's command line.  Figure~\ref{fg:density-map-file}
shows the first six lines of an example file.  In this example, all
HMMs have three states and all states have their own mixture,
indicated by the data constructor \verb~Mix~.  If the density of any
state was {\em tied\/} to that of another state
(Chapter~\ref{ch:HmmDensities}), the constructor \verb~Mix~ would be
replaced by the constructor \verb~TiedM~ along with the
``targeted'' phone and state values.  The targeted HMM state must have
its own mixture; that is, a state must either have its own density or
be tied to a state that does, no multiple chaining of ties is allowed.
(This program does not explicitly check for this, it is the
responsibility of the user to make sure the density map file is
correctly structured).
        \begin{figure}
        \begin{verbatim}
                (AA,    [1 =: Mix, 2 =: Mix, 3 =: Mix])
                (AE,    [1 =: Mix, 2 =: Mix, 3 =: Mix])
                (AH,    [1 =: Mix, 2 =: Mix, 3 =: Mix])
                (AO,    [1 =: Mix, 2 =: Mix, 3 =: Mix])
                (AW,    [1 =: Mix, 2 =: Mix, 3 =: Mix])
                (AX,    [1 =: Mix, 2 =: Mix, 3 =: Mix])
        \end{verbatim}
        \caption[]{The first six lines of an example density map file.}
        \label{fg:density-map-file}
        \end{figure}
        \begin{haskell}{DensityMap}

> data DensityMap = Mix | TiedM Phone HmmState  deriving (Read, Show)

\end{haskell}


        The function \verb~restructure~ restructures the input list.
Hence, the lines shown in Figure~\ref{fg:density-map-file} would be
restructured as shown in Figure~\ref{fg:restructure}.
        \begin{figure}
        \begin{verbatim}
            [(AA, 1 =: Mix), (AA, 2 =: Mix), (AA, 3 =: Mix)]
            [(AE, 1 =: Mix), (AE, 2 =: Mix), (AE, 3 =: Mix)]
            [(AH, 1 =: Mix), (AH, 2 =: Mix), (AH, 3 =: Mix)]
            [(AO, 1 =: Mix), (AO, 2 =: Mix), (AO, 3 =: Mix)]
            [(AW, 1 =: Mix), (AW, 2 =: Mix), (AW, 3 =: Mix)]
            [(AX, 1 =: Mix), (AX, 2 =: Mix), (AX, 3 =: Mix)]
        \end{verbatim}
        \caption[]{The results of applying the function {\tt
restructure} to each of the first six lines of the example density map
file.}
        \label{fg:restructure}
        \end{figure}
        \begin{haskell}{restructure}

> restructure :: (Phone, [Assoc HmmState DensityMap]) ->
>                [(Phone, Assoc HmmState DensityMap)]
> restructure (p,is) = [(p,k) | k<-is ]

\end{haskell}
        These individual lists can be concatenated to form the density
map structure that is used to read a set of Gaussian mixture files.


%----------------------------------------------------------------------
\section {Building the Tied Mixture Table}
\label{sc:tmt}
%----------------------------------------------------------------------

        The mixtures are stored in separate files, one mixture per
file, all in the same directory.  The name of that directory is the
first argument on this program's command line.  Each file has a name
with the syntax
        \begin{quote}\it
        $<$phone$>$.$<$state$>$.gm
        \end{quote}
        where {\it $<$phone$>$} is one of the legal values of
\verb~Phone~ and {\it $<$state$>$} is the state index.  Keeping the
densities in separate files is natural since the densities are
estimated individually by collecting all the feature vectors that
aligned with a given HMM state (or those that are tied to it) into a
single file\footnote{This redistributing of feature vectors is done
using a C program not described in this report.} and then estimating
the density parameters using a C program (not described in this
report).


        The function \verb~get_gm_fname~ is used to build the filename
of a file containing Gaussian mixture parameters.
        \begin{haskell}{get_gm_fname}

> get_gm_fname         :: String -> Phone -> Int -> String
> get_gm_fname dir p k =  dir ++ "/" ++ shows p ('.' : shows k ".gm")

\end{haskell}


        The function \verb~build_tmt~ builds the tied mixture table.
If the density for HMM $p$, state $k$, is tied to another mixture,
then we just pass that information through.  If, however, the density
for HMM $p$, state $k$, is its own mixture, then we open the
appropriate file and read the mixture parameters, converting them to
an internal representation to improve efficiency
(Chapter~\ref{ch:HmmDensities}).  The variable ``\verb~tc~'' in the
definition stands for ``tied-mixture continuation.''
        \begin{haskell}{build_tmt}

>#if __HASKELL1__ < 3
> build_tmt :: String ->
>              [(Phone, Assoc HmmState DensityMap)] ->
>              [Assoc Phone (Assoc Int TiedMixture)] ->
>              (TmTable -> Dialogue) ->
>              Dialogue

> build_tmt dir ((p,k:=t):rps) as tc =
>       case t of
>       TiedM q s -> build_tmt  dir  rps  ((p:=(k:=Tie q s)):as)  tc
>       Mix       -> let
>                      file = get_gm_fname  dir  p  k
>                    in
>                      readFile  file  exit  $ \bs ->
>                      case  readMixture bs  of 
>                      Nothing      -> error (can't_read file)
>                      Just (m,bs') -> if null bs'
>                                      then let
>                                             m' = extern_to_intern m
>                                           in
>                                             build_tmt dir rps
>                                               ((p:=(k:=Gm m')):as) tc
>                                      else error (can't_read file)

> build_tmt  _ [] as tc = tc (make_tm_table as)

>#else
> build_tmt :: String ->
>              [(Phone, Assoc HmmState DensityMap)] ->
>              [Assoc Phone (Assoc Int TiedMixture)] ->
>              IO TmTable

> build_tmt dir ((p,(k,t)):rps) as =
>       case t of
>       TiedM q s -> build_tmt  dir  rps  ((p=:(k=:Tie q s)):as)
>       Mix       -> let
>                      file = get_gm_fname  dir  p  k
>                    in
>                      readFile  file  >>= \bs ->
>                      case  readMixture bs  of 
>                      Nothing      -> error (can't_read file)
>                      Just (m,bs') -> if null bs'
>                                      then let
>                                             m' = extern_to_intern m
>                                           in
>                                             build_tmt dir rps
>                                               ((p=:(k=:Gm m')):as)
>                                      else error (can't_read file)

> build_tmt  _ [] as = return (make_tm_table as)

>#endif

> can't_read :: String -> String
> can't_read file = " can't read the file " ++ file

> make_tm_table = amap (\as -> array (1, length as) as) .
>                 accumArray (flip (:)) [] phone_bounds

\end{haskell}

%----------------------------------------------------------------------
\section {Performing the Viterbi Alignment}
%----------------------------------------------------------------------

        \begin{haskell}{align_each_file}

>#if __HASKELL1__ < 3
> align_each_file :: TmTable ->
>                    HmmNetworkDic ->
>                    Int ->             -- number of files aligned
>                    Float ->           -- cumulative alignment score
>                    StrListCont

> align_each_file  hmm_tms  hmm_dgs  nfs  ts  (fn:rfns) =
>       readFile (fn ++ ".ppm") exit           $ \cs ->
>       readFile (fn ++ ".fea") exit           $ \bs ->
>       let
>         Just (pnet,_)  = readsPrnNetwork cs
>         hmm            = buildHmm hmm_dgs pnet
>         fvs            = readVectors observation_dimen bs
>         lts            = map (eval_log_densities hmm_tms) fvs
>         (score,states) = align hmm lts
>         nfs'           = nfs + 1
>         ts'            = ts  + score
>       in
>         appendChan stderr (printf "%4d%7.2f%7.2f  %s\n" [
>           UInt nfs', UFloat score, UFloat (ts' / fromIntegral nfs'),
>           UString fn ]) exit $
>         writeFile (fn ++ ".algn") (showAlignment states) exit 
>           (align_each_file  hmm_tms  hmm_dgs  nfs' ts' rfns)

> align_each_file _ _ _ _ [] = done

>#else
> align_each_file :: TmTable ->
>                    HmmNetworkDic ->
>                    Int ->             -- number of files aligned
>                    Float ->           -- cumulative alignment score
>		     [String] ->	-- file names
>                    IO ()

> align_each_file  hmm_tms  hmm_dgs  nfs  ts  (fn:rfns) =
>       readFile (fn ++ ".ppm") >>=           \cs ->
>       readFile (fn ++ ".fea") >>=           \bs ->
>       let
>         Just (pnet,_)  = readsPrnNetwork cs
>         hmm            = buildHmm hmm_dgs pnet
>         fvs            = readVectors observation_dimen bs
>         lts            = map (eval_log_densities hmm_tms) fvs
>         (score,states) = align hmm lts
>         nfs'           = nfs + 1
>         ts'            = ts  + score
>       in
>         hPutStr stderr (printf "%4d%7.2f%7.2f  %s\n" [
>           UInt nfs', UFloat score, UFloat (ts' / fromIntegral nfs'),
>           UString fn ]) >>
>         writeFile (fn ++ ".algn") (showAlignment states) >>
>         align_each_file  hmm_tms  hmm_dgs  nfs' ts' rfns

> align_each_file _ _ _ _ [] = return ()

>#endif

> showAlignment :: [(Phone, HmmState)] -> String
> showAlignment = unlines . map showIndexedState . zip [0..]


> showIndexedState (i, x) = shows i (' ' : showState x)


> showState (p, j) = show p ++ " " ++ show j

\end{haskell}
