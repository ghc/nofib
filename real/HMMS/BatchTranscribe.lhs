        The program ``Transcribe'' is fine for checking the
pronunciation modeling functions on single files.  But it would be
inefficient to use ``Transcribe'' to generate the phonetic
pronunciation models for all of the training texts because it reads
the pronunciation dictionary each time it runs.  The program
``BatchTranscribe'' is designed to work efficiently on a large
collection of text files.

        The program takes two arguments.  The first is the
user-supplied dictionary file
(Section~\ref{sc:dictionary}).\footnote{Originally, we tried a
separate program for compiling the balanced binary tree of
pronunciation models.  However, even when the tree was written in
binary form, reading the compiled tree took significantly longer
(almost four times) than just regenerating the tree directly from the
user-supplied dictionary file.  This comparison was for a dictionary
containing about 6200 words.} The second argument is a file that
contains the names of the files that contain the training texts (minus
their ``\verb~.txt~'' extensions).
        \begin{haskell}{BatchTranscribe}

> module Main where

> --partain: import Maybe

> import BalBinSTrees
> import Pronunciations

> main = getArgs exit $ \args ->
>       case args of
>       [dic_file, utt_file] -> process_utts dic_file utt_file
>
>       _                    -> error (" Bad command line\n" ++ usage)


> usage = " usage:  BatchTranscribe  <dictionary>  <utt list file>"


> process_utts dic_file utt_file =
>       readFile dic_file exit  $ \ cs1 ->
>       readFile utt_file exit  $ \ cs2 ->
>       let
>         dictionary = bbstBuild (readDictionary cs1)
>         file_names = lines cs2
>       in
>         foldr (transcribe_file dictionary) done file_names


> transcribe_file dictionary fn d =
>       let
>         in_file  = fn ++ ".txt"
>         out_file = fn ++ ".ppm"
>       in
>         appendChan stderr (in_file ++ "  \t-> ") exit $
>         readFile in_file exit                         $ \ cs ->
>         let
>           network = pre_hmm dictionary cs
>         in
>           writeFile out_file (showPrnNetwork network) exit $
>           appendChan stderr (out_file ++"\n") exit d

\end{haskell}
