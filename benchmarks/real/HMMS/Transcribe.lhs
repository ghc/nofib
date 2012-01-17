        The program ``Transcribe'' is intended for checking the
pronunciation modeling functions on single files.  To generate the
phonetic pronunciation networks for a collection of text files, the
program ``BatchTranscribe'' (Chapter~\ref{ch:BatchTranscribe}) is more
efficient.

        The program takes two arguments.  The first is the
user-supplied dictionary file
(Section~\ref{sc:dictionary}).\footnote{Originally, we tried a
separate program for compiling the balanced binary tree of
pronunciation models.  However, even when the tree was written in
binary form, reading the compiled tree took significantly longer
(almost four times) than just regenerating the tree directly from the
user-supplied dictionary file.  This comparison was for a dictionary
containing about 6200 words.} The second argument is the name of a
text file (minus the ``\verb~.txt~'' extension, which is assumed).
The output will be placed in the file with the second argument as its
root and the extension ``\verb~.ppm~''.
        \begin{haskell}{Transcribe}

> module Main where

> import BalBinSTrees
> import Pronunciations

> main = getArgs exit $ \args ->
>       case args of
>       [dic_file, txt_file] -> process_utt dic_file txt_file
>
>       _  -> error usage

> usage = " usage:  Transcribe  <dictionary>  <txt file>"


> process_utt dic_file txt_file =
>       readFile dic_file exit  $ \ cs1 ->
>       let
>         dictionary = bbstBuild (readDictionary cs1)
>       in
>         transcribe_file dictionary txt_file done


> transcribe_file dictionary fn d =
>       let
>         in_file  = fn ++ ".txt"
>         out_file = fn ++ ".ppm"
>       in
>         readFile in_file exit $ \ cs ->
>         let
>           network = pre_hmm dictionary cs
>         in
>           writeFile out_file (showPrnNetwork network) exit d

\end{haskell}
