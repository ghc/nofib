
        This module defines data structures for representing
pronunciation models of words and utterances.
        \begin{haskell}{Pronunciations}

> module Pronunciations(
>       module Phones,
>       module BalBinSTrees, module MaybeStateT,
>       Word, DigraphNode, Digraph, PrnNetwork(..),
>       DictionaryEntry,
>       readDictionary, readsPrnNetwork, showPrnNetwork,
>       pre_hmm
>       ) where
> import Char(isSpace,toUpper,isDigit)

\end{haskell}


        The following modules are part of a general library and are
described in later chapters in Part~\ref{part:library}.
        \begin{verbatim}

> import BalBinSTrees
> import Lists
> import MaybeStateT
> import PlainTextIO
> import StateT

\end{verbatim}


        The module \verb~Phones~ was defined in
Chapter~\ref{ch:Phones}.
        \begin{verbatim}

> import Phones

\end{verbatim}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Mathematical Representation}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        The pronunciation model for a word is a {\em pronunciation
network}\index{pronunciation network}, consisting of (1) an acyclic
directed graph~\cite{Harary69} called the {\em pronunciation
digraph}\index{pronunciation digraph}, (2) a subset of nodes
designated as the {\em initial nodes}\index{initial nodes}, and (3) a
subset of nodes designated as the {\em terminal nodes}\index{terminal
nodes}. Each node of a pronunciation digraph is associated with some
data, e.g., a subword unit or a hidden Markov model of a subword unit
(Chapter~\ref{ch:HmmDigraphs}).


        As an example, the pronunciation network for ``EXIT'' is shown
in Figure~\ref{fg:dg1-exit}.  We represent the written form of a word
entirely in upper-case letters for consistency with the format used by
the evaluation software provided by the National Institute of
Standards and Technology (NIST)\index{NIST}.  The initial nodes---only
one in this case---are the destination nodes of the arcs eminating
from the smaller filled circle on the left side of the figure.  The
terminal nodes---again, only one in this case---are the starting nodes
of the arcs leading into the smaller filled circle on the right side
of the figure.  In general, a word can have multiple initial nodes,
multiple terminal nodes, or both.  The two smaller filled circles are
used only for displaying the network structure; they do not represent
nodes in the network. In Figure~\ref{fg:dg1-exit}, the node data are
data constructors of the type \verb~Phone~, which was defined in
Chapter~\ref{ch:Phones}.
        \begin{haskell}{Word}

> type Word = String

\end{haskell}
        \begin{figure}
        \begin{center}
        \input{figs/dg1-EXIT}
        \end{center}
        \caption[]{Pronunciation network for ``EXIT.''}
        \label{fg:dg1-exit}
        \end{figure}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Haskell Representation}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        We need to give a name to each node in order to represent the
pronunciation network in the computer.  We can't use the phones
associated with the nodes because the same phone may appear more than
once within the same word.  Also, the network will undergo a series of
transformations that will change the data values.  Without loss of
generality, we elect to use positive integers to name the nodes.  The
integer assigned to a node is that node's {\em index}\index{index}.
These indices can be used to construct the predecessor list for each
node, which is how we will represent the pronunciation digraph.  To
provide a uniform representation of all the words in the vocabulary
and to simplify some of the algorithms, we will index the digraph
nodes according to the following conventions:
        \begin{enumerate}
        \label{conventions}

        \item Integers are used in ascending order starting from
              some positive integer with no skips. (Within the dictionary
              file, this starting integer will be 1, but
              under more general circumstances the starting integer
              could be something different.)

        \item A node's index is always larger than the
              indices of all the nodes that precede it in the
              digraph.  A node $x$ {\em precedes\/} a node $y$ if
              there is a directed path starting at $x$ and ending at
              $y$~\cite[p.\ 198]{Harary69}.  Thus, the acyclic
              digraph is {\em topologically sorted}.

        \end{enumerate}
        Under these conventions, the highest indexed node is always a
terminal node, but not necessarily the only one (in general, words may
have more than one terminal node).  Also, the lowest indexed node is
always an initial node, but, again, not necessarily the only one.


        Following these conventions, the nodes of the pronunciation
network for ``EXIT'' can be indexed as shown in
Figure~\ref{fg:dg2-exit}.  Other numberings are possible.
        \begin{figure}
        \begin{center}
        \input{figs/dg2-EXIT}
        \end{center}
        \caption[]{Pronunciation network for ``EXIT,'' where
each node has been assigned an index according to the conventions
listed in the text.}
        \label{fg:dg2-exit}
        \end{figure}



        A single node of a pronunciation digraph is represented as a
pair.  The first component is the data stored with the node (e.g., the
phone symbol) and the second component is the predecessor list for
that node.  By using a type variable for the node data we provide
ourselves the flexibility of having different digraph types.
        \begin{haskell}{DigraphNode}

> type DigraphNode a = (a, [Int])

\end{haskell}


        There are two obvious possibilities for storing the collection
of digraph nodes; we could use either a list or an array.  For
building composite pronunciation networks we will not need to randomly
access the digraph nodes, so it will be convenient to just use lists.
        \begin{haskell}{Digraph}

> type Digraph a = [DigraphNode a]

\end{haskell}


        The digraph nodes are placed in the list in the order of their
indices.  Thus, the index for each node is implicitly encoded by its
position in the list, although it should be remembered that Haskell
lists are indexed from zero while we are indexing digraph nodes
starting with 1 in the dictionary and possibly something else in other
contexts.  Our digraph representation for ``EXIT'' is shown in
Figure~\ref{fg:dgHaskell-EXIT}.
        \begin{figure}
        \begin{verbatim}
 [(EH, []), (K, [1]), (S, [2]), (G, [1]), (Z, [4]), (IX, [3,5]), (T, [6])]
\end{verbatim}
        \caption[]{Digraph representation for ``EXIT.''}
        \label{fg:dgHaskell-EXIT}
        \end{figure}


        The \verb~Digraph~ data structure doesn't include information
about the initial and terminal nodes, the information needed to
completely specify a pronunciation network.  To include this
information, the pronunciation network is represented using an
algebraic datatype.  The data constructor \verb~PrnN~ (for
``pronunciation network'') takes four arguments: the highest node
index used in the digraph representation (this equals the number of
nodes when the lowest index is 1), the list of initial nodes, the list
of terminal nodes, and the list-based digraph representation.  We make
the data type inherit properties from the class \verb~Text~ so we can
easily read and write the representation as plain text.
        \begin{haskell}{PrnNetwork}

> data PrnNetwork a = PrnN Int [Int] [Int] (Digraph a)  deriving Show{-was:Text-}

\end{haskell}
        The Haskell representation for the pronunciation network for
``EXIT'' is shown in Figure~\ref{fg:pm-EXIT}.
        \begin{figure}
        \begin{verbatim}
     PrnN 7 [1] [7] [(EH, []), (K, [1]), (S, [2]), (G, [1]), (Z, [4]),
                     (IX, [3,5]), (T, [6])]
\end{verbatim}
        \caption[]{Pronunciation network representation for ``EXIT.''}
        \label{fg:pm-EXIT}
        \end{figure}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {The User-Supplied Dictionary File}
\label{sc:dictionary}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        The word ``EXIT'' can be represented in a dictionary file as
shown in Figure~\ref{fg:dic-exit}.  First there is a line containing
the word itself.  Then there is a line with three fields: the total
number of nodes in the network, the list of initial node indices, and
the list of terminal node indices.  This is followed by a series of
lines, one for each node.  Each node-description line contains the
node index, followed by the phone symbol, followed by the predecessor
list for that node.  The first field of each node-description
line---the node index---is not really needed, but we include it in the
file as an aid to checking and modifying word pronunciation networks
by hand.
        \begin{figure}
        \begin{center}
        \begin{verbatim}
                         EXIT
                         7  [1]  [7]
                         1       EH      []
                         2       K       [1]
                         3       S       [2]
                         4       G       [1]
                         5       Z       [4]
                         6       IX      [3,5]
                         7       T       [6]
\end{verbatim}
        \end{center}
        \caption[]{Dictionary file entry for the word ``EXIT.''}
        \label{fg:dic-exit}
        \end{figure}


        An example of a dictionary file containing the words ``EXIT,''
``NOW,'' and ``PLEASE'' is shown in Figure~\ref{fg:dictionary}.  Word
representations are separated from each other by blank lines.
        \begin{figure}
        \begin{center}
        \begin{verbatim}
                         EXIT
                         7  [1]  [7]
                         1       EH      []
                         2       K       [1]
                         3       S       [2]
                         4       G       [1]
                         5       Z       [4]
                         6       IX      [3,5]
                         7       T       [6]

                         NOW
                         2  [1]  [2]
                         1       N       []
                         2       AW      [1]

                         PLEASE
                         4  [1]  [4]
                         1       P       []
                         2       L       [1]
                         3       IY      [2]
                         4       Z       [3]

\end{verbatim}
        \end{center}
        \caption[]{An example of a small dictionary file.}
        \label{fg:dictionary}
        \end{figure}


        Thus, the user-provided dictionary file is a plain text file
that contains the pronunciation networks for all the words in the
vocabulary.  The program \verb~ConvertLinearDic~
(Chapter~\ref{ch:ConvertLinearDic}) can be used to convert a simpler
form of dictionary file to this format.


        We now define the functions used to read the dictionary file.
We begin with the following type synonym.  A {\em dictionary entry\/}
is a word paired with its pronunciation network.
        \begin{haskell}{DictionaryEntry}

> type DictionaryEntry a = (Word, PrnNetwork a)

\end{haskell}


        The function \verb~readDictionary~ takes the contents of the
dictionary file and returns a list of the entries.  Later, these
entries could be stored in a data structure that provides more
efficient retrieval than is possible using a linear list.  For
example, the programs ``Transcribe'' and ``BatchTranscribe'' described
in Chapters~\ref{ch:Transcribe} and~\ref{ch:BatchTranscribe} will
convert the dictionary to a balanced binary search tree.  Because of
the tree-building algorithm used, the entries in the user-supplied
dictionary file do not have to be in alphabetical order.
        \begin{haskell}{readDictionary}

> readDictionary :: [Char] -> [DictionaryEntry Phone]
> readDictionary cs =
>       let
>         (w, cs') = break isSpace (dropWhile isSpace cs)
>       in
>         if null w
>         then  []
>         else  case  readsPrnNetwork cs'  of
>               Nothing         -> error ("readDictionary: can't read \
>                                         \the pronunciation network \
>                                         \for `" ++ w ++ "'")
>               Just (pn, cs'') -> (w, pn) : readDictionary cs''

\end{haskell}
        In the definition of \verb~readDictionary~, we get the written
form of the word via the expression \verb~break isSpace (... cs)~
instead of using the Standard Prelude function \verb~lex~ because we
want to allow some characters in our words that aren't handled the way
we want by \verb~lex~, e.g., hyphens.


        The function \verb~readsPrnNetwork~ reads the pronunciation
network information following a word.  We use a ``Maybe State
Transformer'' monad (Chapter~\ref{ch:MaybeStateT}) to structure the
code.  We also make the function polymorphic in the node data,
requiring only that it belong to the class \verb~Text~ so that
pronunciation networks containing different types of node data can be
read.  The function \verb~readsItem~ is defined in the module
\verb~PlainTextIO~ (Chapter~\ref{ch:PlainTextIO}).
        \begin{haskell}{readsPrnNetwork}
 
> readsPrnNetwork :: (Read d) => MST [Char] (PrnNetwork d)
> readsPrnNetwork = readsItem           `thenMST`  \ n  ->
>                   readsItem           `thenMST`  \ is ->
>                   readsItem           `thenMST`  \ ts ->
>                   readsDigraph n      `thenMST`  \ dg ->
>                   returnMST (PrnN n is ts dg)

\end{haskell}


        The function \verb~readsDigraph~ reads in the data for each
node in the digraph.  This function is also polymorphic in the type of
the node data.  The first argument, \verb~n~, is the number of nodes
in the digraph.
        \begin{haskell}{readsDigraph}

> readsDigraph :: (Read d) => Int -> MST [Char] (Digraph d)
> readsDigraph n = if n <= 0
>                  then returnMST []
>                  else readsNode            `thenMST` \ node ->
>                       readsDigraph (n-1)   `thenMST` \ rdg ->
>                       returnMST (node : rdg)

\end{haskell}


        The function \verb~readsNode~ reads the information for a
single node of the pronunciation digraph.  The function
\verb~readsInt~ is defined in the module \verb~PlainTextIO~
(Chapter~\ref{ch:PlainTextIO}).  We need it because the node index is
read but discarded by this function, so the type checker wouldn't know
that \verb~readsItem~ was supposed to be looking for an \verb~Int~.
Thus, we need to be explicit about the fact that the first item to be
read is an \verb~Int~.
        \begin{haskell}{readsNode}

> readsNode :: (Read d) => MST [Char] (DigraphNode d)
> readsNode = readsInt          `thenMST` \ _  ->       -- node index
>             readsItem         `thenMST` \ d  ->       -- node data
>             readsItem         `thenMST` \ preds ->    -- pred list
>             returnMST (d, preds)

\end{haskell}


        For example, applying \verb~readDictionary~ to the file
shown in Figure~\ref{fg:dictionary} yields the list
        \begin{verbatim}
     [ ( "EXIT",   PrnN 7 [1] [7] [(EH,[]), (K,[1]), (S,[2]),
                                   (G,[1]), (Z,[4]), (IX,[3,5]),
                                   (T,[6])] ),
       ( "NOW",    PrnN 2 [1] [2] [(N,[]), (AW, [1])] ),
       ( "PLEASE", PrnN 4 [1] [4] [(P,[]), (L,[1]), (IY,[2]),
                                   (Z,[3])] )
     ]
\end{verbatim}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Building Pronunciation Networks for Complete Utterances}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        We need to build pronunciation networks for complete
utterances, not just single words.  This construction is performed as
a series of steps.  In the following list, the name of the
corresponding function is shown in parentheses.
        \begin{itemize}

        \item Convert the text to a canonical form
(\verb~prepare_text~, Section~\ref{sb:preparetext}).

        \item Break the text into individual words (\verb~words~,
Standard Prelude).

        \item Look up each word in the pronunciation dictionary
(\verb~look_up_words~, Section~\ref{sb:lookup}).

        \item Place an optional between-word model
(Section~\ref{sb:betweenword}) at the beginning of the utterance and
after each word.

        \item Reindex each pronunciation network
(\verb~reindexPrnNetworks~, Section~\ref{sb:reindex}).

        \item Join the individual models into a single network model
for the entire utterance.\footnote{In the first version of this
module, the reindexing and joining was done within one function.
While simple, this approach resulted in an algorithm with quadratic
complexity.}

        \end{itemize}


        The flow of processing is summarized in
Figure~\ref{fg:overall-flow}.  As a concrete example, the models for
the words ``PLEASE,'' ``EXIT,'' and ``NOW'' and for the utterance
``PLEASE EXIT NOW'' are shown in Figure~\ref{fg:please-exit-now}.
        \begin{figure}
        \begin{center}
\begin{tabular}{cp{2.3in}}
\hline
\rule{0in}{3ex} $Text$ & \\
$\Downarrow$ & \\
$Text'$ & Convert to canonical form\\
$\Downarrow$ & \\
$[w_1,w_2,\ldots,w_L]$ & break into individual words\\
$\Downarrow$ & \\
$[P_{w_1},P_{w_2},\ldots,P_{w_L}]$ & look up the pronunciation models \\
$\Downarrow$ & \\
$[P_{bwm},P_{w_1},P_{bwm},P_{w_2},P_{bwm},\ldots,P_{bwm},P_{w_L},P_{bwm}]$
& interleave the between-word model \\
$\Downarrow$ & \\
$[P'_{bwm},P'_{w_1},P'_{bwm},P'_{w_2},P'_{bwm},\ldots,P'_{bwm},P'_{w_L},P'_{bwm}]$
& reindex each network \\
$\Downarrow$ & \\
$P'_{bwm} \oslash ( (P'_{w_1} \oplus P'_{bwm}) \otimes \ldots \otimes
                    (P'_{w_L} \oplus P'_{bwm}) )$
& reduce to a single network for the entire utterance \\
$\Downarrow$ & \\
$P_{Text}$  & \\[0.10in]
\hline
\end{tabular}
\end{center}
        \caption[]{Overall flow of processing.  $P_{bwm}$ denotes the
pronunciation network of the between-word model and $P_{Text}$ denotes
the pronunciation network for the utterance text.  The operators
$\oslash$, $\oplus$, and $\otimes$ correspond to the functions
\verb~joinNets1~, \verb~joinNets2~ and \verb~joinNets~, respectively,
(Section~\ref{sb:joining}).}
        \label{fg:overall-flow}
        \end{figure}


        \begin{figure}
        %
        \begin{center}
        \subfigure[``PLEASE'']{%
        \hbox to 4.00in{
        \input{figs/dg-PLEASE}}}
        \end{center}
        %
        \begin{center}
        \subfigure[``EXIT'']{%
        \input{figs/dg2-EXIT}}
        \end{center}
        %
        \begin{center}
        \subfigure[``NOW'']{%
        \hbox to 4.00in{
        \input{figs/dg-NOW}}}
        \end{center}
        %
        \begin{center}
        \subfigure[``PLEASE EXIT NOW.'']{%
        \input{figs/dg-PLEASEEXITNOW}}
        \end{center}
        %
        \caption[]{Pronunciation networks for the words ``PLEASE,''
``EXIT'' and ``NOW,'' and for the complete utterance ``PLEASE EXIT
NOW.''  The model for the complete utterance includes an optional
silence model at the beginning, end, and between each word.}
        \label{fg:please-exit-now}
        \end{figure}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {Converting the Text to a Canonical Form}
\label{sb:preparetext}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        The first task is to transform the text string into a
canonical form.  We define a function \verb~prepare_text~ to perform
this transformation.  The function implements the following steps:
        \begin{itemize}

        \item Sometimes the transcription file contains two integers
preceding the text of the utterance (e.g., the TIMIT ``\verb~.txt~''
transcription files).  These numbers represent the starting and
stopping sample numbers for the entire utterance.  We need to remove
them and any white space that preceeds the text.  Note that the text
itself cannot start with a digit or the function \verb~prepare_text~
will remove that too!

        \item Drop any word-external punctuation
characters.\footnote{Since we control the contents of the textual
transcription files, we can forbid abbreviations like `e.g.,', `c.f.,'
etc., requiring that they be spelled out.  This requirement allows us
to remove all periods without having to check to see if they are part
of an abbreviation.}  We retain some characters, like hyphens,
underscores, and tildes.

        \item Change all of the letters in the string to upper-case.
Each word is represented in the dictionary in only one form, and we
have chosen to use all upper-case letters since that is consistent
with NIST's ``snr'' format.

        \end{itemize}
        \begin{haskell}{prepare_text}

> prepare_text = map toUpper . 
>                  filter (`notElem` "!,.:;?") .
>                    dropWhile (\c -> isDigit c || isSpace c)

\end{haskell}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {Looking Up Pronunciation Models}
\label{sb:lookup}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        \begin{haskell}{look_up_words}

> look_up_words :: BalBinSTree Word (PrnNetwork Phone) ->
>                  [Word] ->
>                  [PrnNetwork Phone]

> look_up_words dictionary = map (bbstLookUp dictionary)

\end{haskell}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {A Between-Word Model}
\label{sb:betweenword}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        We create a between-word model to be placed before and at the
end of an utterance as well as between each word of an utterance.  For
now this is just silence, represented by the constructor
\verb~SIL~. Later, we might want to enhance the model to include
filled pauses, e.g., ``ums'' and ``ahs.''
        \begin{haskell}{between_word_model}

> between_word_model = PrnN 1 [1] [1] [(SIL,[])]

\end{haskell}
        The function \verb~interleave~ in the library module
\verb~Lists~ (Chapter~ref{ch:Lists}) can be used to interleave the
between-word model through the list of words.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {Reindexing Pronunciation Networks}
\label{sb:reindex}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        The function \verb~reindexPrnNetwork~ is used to increment the
node indices by a fixed amount.  It returns the newly indexed network
and the sum of the increment and the old highest index value.  The
function uses a State Transformer monad (Chapter~\ref{ch:StateT}).
        \begin{haskell}{reindexPrnNetwork}

> reindexPrnNetwork :: (PrnNetwork a) -> ST Int (PrnNetwork a)
> reindexPrnNetwork (PrnN n is ts dg) inc = (PrnN n' is' ts' dg', n')
>       where
>       n'        = n + inc
>       increment = (inc+)
>       is'       = map increment is
>       ts'       = map increment ts
>       dg'       = mapsnd (map increment) dg

\end{haskell}


        The function \verb~reindexPrnNetworks~ is used to reindex
every pronunciation network in a list of such networks, starting with
an increment of 0.  Thus, the first network in the list will be
unchanged, the second network's indices will be incremented by the
number of nodes in the first network, the third network's indices will
be incremented by the number of nodes in the first two, etc.  The
functions \verb~maplST~ and \verb~startingFrom~ are defined in the
module \verb~StateT~ (Chapter~\ref{ch:StateT}).
        \begin{haskell}{reindexPrnNetworks}

> reindexPrnNetworks :: [PrnNetwork a] -> [PrnNetwork a]
> reindexPrnNetworks ps = maplST reindexPrnNetwork ps `startingFrom` 0

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {Joining Pronunciation Networks}
\label{sb:joining}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        We now define three functions for joining pronunciation
networks.  Note that in all three cases, the functions assume that the
second network of the pair has been properly reindexed so that the new
combination satisfies our indexing conventions
(page~\pageref{conventions}).  The required condition is that the
lowest index of the second network equals the highest index of the
first network plus 1.



        The function \verb~joinNets~ joins two pronunciation networks
into a single pronunciation model where neither network is optional.
The initial nodes of the new network are those of the first, the
terminal nodes are those of the second, and every node that is an
initial node of the second network has its predecessor list augmented
by the terminal nodes of the first.
        \begin{haskell}{joinNets}

> joinNets :: PrnNetwork a -> PrnNetwork a -> PrnNetwork a

> joinNets (PrnN n1 is1 ts1 dg1) (PrnN n2 is2 ts2 dg2) =
>       PrnN n2 is1 ts2 dg'
>       where
>       dg' = dg1 ++ [ if k `elem` is2
>                         then  (d, ts1 ++ ps)
>                         else  (d,     ps   )
>                      | (k, (d,ps)) <- zip [n1+1..] dg2 ]

\end{haskell}


        We need another version of this function for joining two word
models when the second word of the pair is optional but the first word
is not, e.g., when the second word is an optional ``between-word''
model (Section~\ref{sb:betweenword}).  The only difference between
\verb~joinNets~ and \verb~joinNets2~ is that \verb~joinNets2~ produces
a different terminal node list; all of the terminal nodes of the first
network are also terminal nodes of the new network.
        \begin{haskell}{joinNets2}

> joinNets2 :: PrnNetwork a -> PrnNetwork a -> PrnNetwork a

> joinNets2 (PrnN n1 is1 ts1 dg1) (PrnN n2 is2 ts2 dg2) =
>       PrnN n2 is1 (ts1 ++ ts2) dg'
>       where
>       dg' = dg1 ++ [ if k `elem` is2
>                         then  (d, ts1 ++ ps)
>                         else  (d,     ps   )
>                      | (k, (d,ps)) <- zip [n1+1..] dg2 ]


\end{haskell}

        Finally, we need still another version for joining two
networks when the first network of the pair is optional but the second
is not.
        \begin{haskell}{joinNets1}

> joinNets1 :: PrnNetwork a -> PrnNetwork a -> PrnNetwork a

> joinNets1 (PrnN n1 is1 ts1 dg1) (PrnN n2 is2 ts2 dg2) =
>       PrnN n2 (is1 ++ is2) ts2 dg'
>       where
>       dg' = dg1 ++ [ if k `elem` is2
>                         then  (d, ts1 ++ ps)
>                         else  (d,     ps   )
>                      | (k, (d,ps)) <- zip [n1+1..] dg2 ]

\end{haskell}

        In the future these functions will be modified to incorporate
word-junction phonological rules\index{phonological rules}.


        In order to carry out the last transformation shown in
Figure~\ref{fg:overall-flow}, we use a two-operator
generalization of the Standard Prelude function \verb~foldr1~ called
\verb~foldr1_2op~, defined in the module \verb~Lists~
(Chapter~\ref{ch:Lists}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {Modeling an Utterance}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        We now take the tools developed so far and define the
functions that will take a string like ``PLEASE EXIT NOW'' and return
its phonetic pronunciation network.  We will also define a function
that modifies the network in preparation for the use of hidden Markov
models.  In that version of the network, the node data will be an
ordered pair.  The first component of the pair will be the phone and
the second component will be the number of successors of that node in
the pronunciation digraph.  The number of successors is used to adjust
the exit probabilities of the corresponding HMM.


        The function \verb~build_pron_network~ implements these steps:
        \begin{haskell}{build_pron_network}

> build_pron_network :: BalBinSTree Word (PrnNetwork Phone) ->
>                       String ->
>                       PrnNetwork Phone

> build_pron_network dictionary =
>         (\(n:rns) -> n `joinNets1` (foldr1_2op joinNets2 joinNets rns))
>       . reindexPrnNetworks
>       . interleave between_word_model
>       . look_up_words dictionary
>       . words

\end{haskell}


        In order to properly adjust the exit probabilities for the
phonetic HMMs, we need to know the number of successors for each node
in the pronunciation digraph.  This number is made part of the node
data; specifically, it becomes the second component of an ordered
pair.
        \begin{haskell}{add_num_succs}

> add_num_succs :: PrnNetwork a -> PrnNetwork (a, Int)

> add_num_succs  (PrnN n is ts dg)  =  PrnN n is ts dg'
>       where
>       dg' = add_num_succs' 1 dg  -- recall that the first node is
>                                  --   indexed with a `1'

> add_num_succs'  indx  ((d,preds):remnodes) =
>       ((d, nsuccs),preds) : add_num_succs' (indx + 1) remnodes
>       where
>       (_, predls) = unzip remnodes
>       nsuccs      = length (filter (indx `elem`) predls)

> add_num_succs' indx [] = []

\end{haskell}


        A flow diagram of the processing up to this point is shown in
Figure~\ref{fg:flow-upto-hmms}.
\begin{figure}
\begin{center}
        {\tt "PLEASE EXIT, NOW."}
        \[
        \Downarrow
        \]
        {\tt ["PLEASE", "EXIT,", "NOW."]}
        \[
        \Downarrow
        \]
        {\tt ["PLEASE", "EXIT", "NOW"]}
        \[
        \Downarrow
        \]
        \input{figs/word_pieces}
        \[
        \Downarrow
        \]
        \input{figs/dg-PLEASEEXITNOW}
        \vspace{-0.50in}
        \[
        \Downarrow
        \]
        \input{figs/dg2-PLEASEEXITNOW}
        \end{center}
        \caption[]{An example showing the flow of processing up to the
point at which we are ready to incorporate the HMMs.}
        \label{fg:flow-upto-hmms}
\end{figure}

        For the program ``BatchTranscribe''
(Chapter~\ref{ch:BatchTranscribe}) it will be useful to combine these
functions into a single function that performs all the steps.
        \begin{haskell}{pre_hmm}

> pre_hmm dic = add_num_succs . build_pron_network dic . prepare_text

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Pretty-Printing a Pronunciation Network}
\label{sc:pprintNetwork}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        It is useful to have a function to print easily readable
representations of pronunciation digraphs whenever the node data is in
the class \verb~Text~.  The function \verb~showPrnNetwork~ prints a
list-based pronunciation network in the same format as it would appear
for a word in the dictionary file (see Section~\ref{sc:dictionary}).
        \begin{haskell}{showPrnNetwork}

> showPrnNetwork  (PrnN n is ts dg) =
>       show n ++ "  " ++ show is ++ "  " ++ show ts ++ "\n"  ++ 
>         unlines (map showIndexedDigraphNode (zip [1..] dg))

\end{haskell}
\fixhaskellspacing\begin{haskell}{showIndexedDigraphNode}

> showIndexedDigraphNode :: (Show d) => (Int, DigraphNode d) -> String
> showIndexedDigraphNode (n,(d,ps)) =
>       shows n ('\t' : shows d ("  \t" ++ show ps))

\end{haskell}


        Using this pretty-printing function, the results of processing
the text ``SHOW DEPARTURES FROM ATLANTA FOR AMERICAN,'' one of the Air
Travel Information System (ATIS) sentences from the ATIS2 database
distributed by the Linguistic Data Consortium (LDC), is shown in
Figure~\ref{fg:atis2-example}.
        \begin{figure}
        \begin{verbatim}
                 40  [1, 2]  [39, 40]
                 1      (SIL, 1)        []
                 2      (SH, 1)         [1]
                 3      (OW, 2)         [2]
                 4      (SIL, 1)        [3]
                 5      (D, 1)          [3, 4]
                 6      (AX, 1)         [5]
                 7      (P, 1)          [6]
                 8      (AA, 1)         [7]
                 9      (R, 1)          [8]
                 10     (CH, 1)         [9]
                 11     (AXR, 1)        [10]
                 12     (Z, 2)          [11]
                 13     (SIL, 1)        [12]
                 14     (F, 1)          [12, 13]
                 15     (R, 1)          [14]
                 16     (AH, 1)         [15]
                 17     (M, 2)          [16]
                 18     (SIL, 1)        [17]
                 19     (AE, 1)         [17, 18]
                 20     (T, 1)          [19]
                 21     (L, 1)          [20]
                 22     (AE, 1)         [21]
                 23     (N, 1)          [22]
                 24     (T, 1)          [23]
                 25     (AX, 2)         [24]
                 26     (SIL, 1)        [25]
                 27     (F, 2)          [25, 26]
                 28     (AO, 1)         [27]
                 29     (R, 2)          [28]
                 30     (AXR, 2)        [27]
                 31     (SIL, 1)        [29, 30]
                 32     (AX, 1)         [29, 30, 31]
                 33     (M, 1)          [32]
                 34     (EH, 1)         [33]
                 35     (R, 1)          [34]
                 36     (IX, 1)         [35]
                 37     (K, 1)          [36]
                 38     (IX, 1)         [37]
                 39     (N, 1)          [38]
                 40     (SIL, 0)        [39]
        \end{verbatim}
        \caption[]{The results of applying the function {\tt pre\_hmm}
to the utterance ``SHOW DEPARTURES FROM ATLANTA FOR AMERICAN'' as
displayed using the pretty-printing function {\tt showPrnNetwork}.}
        \label{fg:atis2-example}
        \end{figure}


%%%%%%%%%%  End of Words.lhs  %%%%%%%%%%
