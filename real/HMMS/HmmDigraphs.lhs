
        This module defines data structures for representing the
topology (i.e., the state transition structure) of hidden Markov
models (HMMs).  The module also defines functions for manipulating
these models so that we can build HMMs of words and utterances.  We
are only concerned with the topology in this module because for the
subword modeling of utterances we do not need to know the nature of
the observations, e.g., whether they are discrete vector-quantizer
indices (e.g., \cite{Lee89}) or continuous-valued vectors (e.g.,
\cite{LeeRabiPierWilp90}).  In this module we are just trying to build
a model that describes the order in which subword events occur, not
their nature.
        \begin{haskell}{HmmDigraphs}

> module HmmDigraphs(
>       module BalBinSTrees,  -- needed for ghc to compile
>       module Phones, module Pronunciations,
>       ProbArc, ProbDigraphNode, ProbDigraphL,
>       ProbDigraphA,
>       HmmNetworkDic,
>       HmmState, HmmData,
>       HmmTsL(..), HmmTsA(..),
>       buildHmm,
>       readHmms, build_hmm_array,
>       get_log_probs, convert_to_log_probs
>       ) where

\end{haskell}

        The following modules are part of a general library and are
described in later chapters in Part~\ref{part:library}.
        \begin{verbatim}

> import BalBinSTrees
> import Lists( mapfst, mapsnd, mapAccumlfst )
> import PlainTextIO
> import Array--1.3
> import Ix--1.3

\end{verbatim}


        The modules \verb~Phones~ and \verb~Pronunciations~ were
defined in Chapters~\ref{ch:Phones} and~\ref{ch:Pronunciations},
respectively.
        \begin{haskell}

> import Phones
> import Pronunciations

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Mathematical Representation}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        Like the pronunciation networks of
Chapter~\ref{ch:Pronunciations}, HMMs have a network structure.
Unlike pronunciation networks, the HMM directed graphs have transition
probabilities assigned to the arcs and the nodes usually have
self-loops. Thus, the representation for HMM networks will be similar
to that for pronunciation networks but slightly more complicated.  We
will call the HMM networks {\em probabilistic
networks\/}\index{probabilistic networks} and the directed graphs {\em
probabilistic digraphs}\index{probabilistic digraphs}.


        An example of a popular phonetic HMM network is shown in
Figure~\ref{fg:phoneHMM1}.  The open circles labeled with numbers
represent the {\em states\/}\index{states} of the HMM.  Thus, the HMM
of Figure~\ref{fg:phoneHMM1} is a 3-state HMM.  The states correspond
to observations.  That is, each state is associated with an
observation probability model.  Such a model might be a probability
mass function over the indices of codewords in a vector quantizer
(e.g., \cite{Lee89}) or a multivariate probability density function
(e.g., \cite{LeeRabiPierWilp90}); the particular form of the
observation model is not important for this module.  The smaller,
filled circle on the left side of the figure is the {\em entry node\/}
and is a useful graphical device for identifying the {\em starting
states}.  Similarly, the smaller, filled circle on the right side of
the figure is the {\em exit node\/} and is useful for identifying the
{\em stopping states}.  Neither the entry node nor the exit node is
associated with an observation model; they are just a convenient
concept for drawing and describing HMM networks.
        \begin{figure}
        \begin{center}
        \input{figs/PhoneHMM1}
        \end{center}
        \caption[]{A popular 3-state phonetic HMM network.}
        \label{fg:phoneHMM1}
        \end{figure}


        With no loss of generality, we can use positive integers to
index each state in order to represent the digraph structure in the
computer.  The numbering conventions proposed in
Chapter~\ref{ch:Pronunciations}, page~\pageref{conventions}, will be
used here as well.


        Another popular phonetic HMM topology is shown in
Figure~\ref{fg:phoneHMM2}.  Unlike that of Figure~\ref{fg:phoneHMM1},
this model doesn't allow the second state to be skipped.
        \begin{figure}
        \begin{center}
        \input{figs/PhoneHMM2}
        \end{center}
        \caption[]{Another popular 3-state phonetic HMM.}
        \label{fg:phoneHMM2}
        \end{figure}
        A popular model for the silence HMM (``\verb~SIL~'') is shown
in Figure~\ref{fg:silhmm}; it has only one state.
        \begin{figure}
        \begin{center}
        \input{figs/sil-hmm}
        \end{center}
        \caption[]{A one-state HMM for silence ``{\tt SIL}''.}
        \label{fg:silhmm}
        \end{figure}


        We want to allow quite general HMM topologies to be
represented.  A more general network that will occasionally be used as
an example in this chapter is shown in Figure~\ref{fg:phoneHMM3}.
This HMM has two starting states and two stopping states.  This model
is not motivated by speech processing considerations, and might turn
out to be a poor choice if used in an automatic speech recognizer.
However, because of the multiple starting and stopping states, this
model is sufficiently complicated to motivate the manner in which some
of this module's functions are defined.
        \begin{figure}
        \begin{center}
        \input{figs/PhoneHMM3}
        \end{center}
        \caption[]{A more general network for a 3-state phonetic HMM.}
        \label{fg:phoneHMM3}
        \end{figure}


        Note that state 2 of the model in Figure~\ref{fg:phoneHMM3} is
both a starting state and a stopping state, even though it can be
preceded by state 1 or followed by state 3 or both.  Thus, to say that
a state is a ``starting state'' means only that it is possible to
start in that state.  It does not mean that we have to start in that
start or that we can't visit that state if we start in a different
one.  Similarly for stopping states.


        The goal of this chapter is to develop functions for building
HMMs of utterances.  The approach is simple.  We assume that we have
an HMM for each subword unit, e.g., phones.  We have already developed
software that takes the text of an utterance and models it as a
network of subword units (Chapter~\ref{ch:Pronunciations}).  We can
replace each subword unit by its HMM to get a network of HMMs, which
can then be reduced to a single HMM for the entire utterance.  For
example, suppose that the HMM network shown in
Figure~\ref{fg:phoneHMM1} is used for all of the phonetic HMMs.  Then
a partial HMM for the word ``EXIT''---which has the pronunciation
network shown in Figure~\ref{fg:dg2-exit}---would be as shown in
Figure~\ref{fg:hmmEXIT}.
        \begin{figure}
        \begin{center}
        \input{figs/HmmEXIT}
        \end{center}
        \caption[]{A partial HMM for the word ``EXIT.''}
        \label{fg:hmmEXIT}
        \end{figure}


        An alternative approach to building a whole-utterance HMM is
to build a pronunciation network containing the subword HMMs and leave
it in this form; that is, don't reduce the network to a single HMM.
However, this would then require a more complicated version of the
Viterbi algorithm (Chapter~\ref{ch:Viterbi}) than we have implemented.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Haskell Representation}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {Probabilistic Arcs}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        An {\em arc\/} is a link between two nodes in a network.  For
HMMs, there is a transition probability assigned to each arc.  Such an
arc is called a {\em probabilistic arc}\index{probabilistic arc}.  A
probabilistic arc is represented as an ordered pair: a predecessor
node index and a transition probability.
        \begin{haskell}{ProbArc}

> type ProbArc = (Int, Float)

\end{haskell}


        There is one exception to the interpretation of the first
coordinate as the index of a predecessor node: for those arcs going
from the entry node into a starting state, the first coordinate is the
starting state index.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {Nodes}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        A node in a probabilistic digraph is represented as a pair.
The first component is the data stored with the node (e.g., a phonetic
symbol, or the parameters that characterize the observation
probability model), and the second component is the predecessor list
for that node.
        \begin{haskell}{ProbDigraphNode}

> type ProbDigraphNode a = (a, [ProbArc])

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {Probabilistic Digraphs: List Version}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        The HMM digraph can be represented by collecting the nodes
together in a list, in the order of their indices.  (This does not
include arcs from the entry node into the starting states or arcs from
the stopping states into the exit node.  It is the marking of certain
states as starting or stopping states that distinguishes a network
from a digraph.)
        \begin{haskell}{ProbDigraphL}

> type ProbDigraphL a = [ProbDigraphNode a]

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {HMM Transition Structure}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        To complete the description of the HMM topology, we need to
include the transitions from the entry node into the starting states,
called the {\em starting arcs}\index{starting arcs}, and those out of
the stopping states into the exit node, called the {\em exiting
arcs}\index{exiting arcs}.  It will also be useful to have the number
of states explicitly present in the data structure.  Thus, the data
structure has four fields, arranged in the following order: the number
of states, the list of starting arcs, the list of exiting arcs, and
the probabilistic digraph. The type inherits the methods of the class
\verb~Text~ for easy reading and writing of the data structure.
        \begin{haskell}{HmmTsL}

> data HmmTsL a = HmmTsL  Int  [ProbArc]  [ProbArc]  (ProbDigraphL a) 
>                 deriving (Read, Show)

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {Probabilistic Digraphs: Array Version}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        Probabilistic directed graphs can also be represented using
arrays for efficient random accessing of node data.  This will be
especially important for the Viterbi alignment algorithm.  The data
types based on arrays are distinguished from their list-based
counterparts by putting an ``A'' at the end of the constructor names
instead of an ``L.''  The array-based structure does not need to have
the number of states explicitly represented since it is present in the
bounds of the array.
        \begin{haskell}{ProbDigraphA}

> type ProbDigraphA a = Array Int (ProbDigraphNode a)

\end{haskell}
\fixhaskellspacing\begin{haskell}{HmmTsA}

> data HmmTsA a = HmmTsA  [ProbArc]  [ProbArc]  (ProbDigraphA a)

\end{haskell}


        Later it will be convenient to have functions for extracting
the different constituents of an object of type \verb~HmmTsA~.
        \begin{haskell}{start_states}

> start_states :: HmmTsA a -> [ProbArc]
> start_states (HmmTsA is _ _ ) = is

\end{haskell}
\fixhaskellspacing\begin{haskell}{stop_states}

> stop_states  :: HmmTsA a -> [ProbArc]
> stop_states  (HmmTsA _ ts _  ) = ts

\end{haskell}
\fixhaskellspacing\begin{haskell}{prb_digraph}

> prb_digraph :: HmmTsA a -> ProbDigraphA a
> prb_digraph (HmmTsA _ _ pdg) = pdg

\end{haskell}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Building an Utterance HMM}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        Assuming that we have an HMM for each phone, we now show how
to take the phonetic pronunciation network of an utterance and build
an HMM for that utterance.  It will be useful to define the following
type synonym.
        \begin{haskell}{HmmState}

> type HmmState = Int

\end{haskell}
        The pair type \verb~(Phone, HmmState)~ occurs so often in the
sequel that we define a type synonym for it as well.
        \begin{haskell}{HmmData}

> type HmmData = (Phone, HmmState)

\end{haskell}


        We need to retrieve the HMM parameters from a look-up table.
This table is implemented as an array.
        \begin{haskell}{HmmNetworkDic}

> type HmmNetworkDic = Array Phone (HmmTsL HmmState)

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {``{\tt lookupHmms}''}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        Looking at the final network in
Figure~\ref{fg:flow-upto-hmms}, we note that each node has a pair of
values as data.  The first is the phone, i.e., which HMM to retrieve
from the HMM look-up table.  The second is the number of successors
for that node in the pronunciation network.  This number is used to
adjust the exit probabilities of the HMM, i.e., the transition
probabilities associated with the exiting arcs.  We now define a
function \verb~lookup_an_hmm~ that retrieves an HMM from the look-up
table, adjusts the exit probabilities, and makes the HMM node data a
pair consisting of the phone and the state index (the node data for
the HMM look-up table entries is just the state index).  Note that
this function assumes that the HMMs in the dictionary use log
probabilities.
        \begin{haskell}{lookup_an_hmm}

> lookup_an_hmm :: HmmNetworkDic -> (Phone, Int) -> HmmTsL HmmData

> lookup_an_hmm  hmm_dg_dic  (p,j)  =  HmmTsL n is ts' dg'
>       where
>       HmmTsL n is ts dg = hmm_dg_dic ! p
>       ts' = if j <= 1
>             then ts        -- 0 or 1 successor nodes
>             else let       -- 2 or more successor nodes
>                    log_of_divisor = log (fromInt j)
>                  in
>                    [(i,ep - log_of_divisor) | (i,ep) <- ts]
>
>       dg' = mapfst (\s -> (p, s)) dg  -- add the phone symbol to the
>                                       -- HMM node data, leaving the
>                                       -- predecessor list unchanged

\end{haskell}


        Now we can take the pronunciation network and change the data
associated with every node from a pair consisting of the phone and the
number of successors to an HMM network with modified exit
probabilities.  Note that the structure of the pronunciation network
is unchanged; only the node data is changed.
        \begin{haskell}{lookupHmms}

> lookupHmms :: HmmNetworkDic ->
>               PrnNetwork (Phone, Int) ->
>               PrnNetwork (HmmTsL HmmData)

> lookupHmms  hmm_dic  (PrnN n is ts dg)  =  PrnN n is ts dg'
>       where dg' = mapfst (lookup_an_hmm hmm_dic) dg

\end{haskell}


        As a concrete example, consider nodes 5, 6 and 7 in the last
pronunciation network of Figure~\ref{fg:flow-upto-hmms}.  The
situation after substituting the HMMs of Figure~\ref{fg:phoneHMM3} for
the node data for these three nodes is shown in
Figure~\ref{fg:initial-hmm-sub}.
        \begin{figure}
        \begin{center}
        \input{figs/InitialHMMSub}
        \end{center}
        \caption[]{A subportion of the pronunciation network
for ``PLEASE EXIT NOW'' with 3-state HMMs as the node data.}
        \label{fg:initial-hmm-sub}
        \end{figure}


        Figure~\ref{fg:final-hmm} shows the portion of the final
whole-utterance HMM that corresponds to the portion of the
pronunciation network shown in Figure~\ref{fg:initial-hmm-sub}
        \begin{figure}
        \begin{center}
        \input{figs/CombinedHMMs}
        \end{center}
        \caption[]{A subportion of the desired whole-utterance HMM.}
        \label{fg:final-hmm}
        \end{figure}
        The remainder of this section is concerned with defining the
functions that are needed to transform the structure shown in
Figure~\ref{fg:initial-hmm-sub} into that of
Figure~\ref{fg:final-hmm}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {``{\tt reindexHmms}''}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        One thing we need to do is change the indices associated with
the nodes of the HMM digraphs, as we did for pronunciation networks.
It will simplify things to do this to each HMM before merging them.
That is, we will first go through the pronunciation network and change
the node indices used within each HMM network representation to the
indices that these nodes will be assigned when they take their place
within the whole-utterance HMM.


        The function \verb~reindex_an_hmm~ takes a probabilistic
network whose nodes are indexed from 1 to $n$ (as is the case for the
HMM networks in the HMM look-up table) and increments them by a
specified amount.  The node data in the digraph is left unchanged.
Thus, even through the nodes of each HMM network are assigned new
indices in order to represent the structure of the final
whole-utterance network, each HMM node still remembers what phonetic
HMM and state it came from.  This is necessary so that we can access
the proper observation probability model during the alignment phase
(Chapter~\ref{ch:Viterbi}).  Furthermore, this function replaces the
list representation of the probabilistic digraph by an array
representation for greater efficiency in the steps that follow.
        \begin{haskell}{reindex_an_hmm}

> reindex_an_hmm :: Int -> HmmTsL a -> (Int, HmmTsA a)

> reindex_an_hmm  m  (HmmTsL n is ts dg)  =  (m_plus_n, HmmTsA is' ts' dg')
>       where
>       m_plus_n = n+m
>       is' = mapfst (m+) is
>       ts' = mapfst (m+) ts
>       dg' = listArray (m+1, m_plus_n) (mapsnd (mapfst (m+)) dg)

\end{haskell}


        We need to re-index every HMM in the pronunciation network.
The key to doing this easily is to use a modified version of the
function \verb~mapAccuml~ that is defined in the system library module
\verb~ListUtil~ provided with the Chalmers compiler.  \verb~mapAccuml~
is an extension of the Standard Prelude function \verb~map~; the
difference is that \verb~mapAccuml~ allows a state to be passed along
the list as part of the calculations.  Our version, called
\verb~mapAccumlfst~ and defined in the library module \verb~Lists~
(Chapter~\ref{ch:Lists}), is essentially the same but operates only
over the first components in a list of pairs, leaving the second
components unchanged.
        \begin{haskell}{reindexHmms}

> reindexHmms :: PrnNetwork (HmmTsL HmmData) ->
>                PrnNetwork (HmmTsA HmmData)

> reindexHmms (PrnN n is ts dg)  =  PrnN n is ts dg'
>       where (_, dg') = mapAccumlfst reindex_an_hmm 0 dg

\end{haskell}


        The re-indexed HMMs for our example subdigraph are shown in
Figure~\ref{fg:reindexed-hmms}.
        \begin{figure}
        \begin{center}
        \input{figs/ReindexedHMMs}
        \end{center}
        \caption[]{Re-indexed HMMs.  Note that the node data for each
HMM state is unchanged.  For example, the node with data {\tt (EH,1)}
will be assigned an index of 19 within the whole-utterance HMM, but it
remembers via the node data that it came from the first state of the
HMM for ``{\tt EH}'' so that the proper observation probability model
can be accessed during Viterbi alignment.}
        \label{fg:reindexed-hmms}
        \end{figure}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {``{\tt combineHmms}''}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        Referring again to Figure~\ref{fg:reindexed-hmms}, we see that
in the whole-utterance HMM the arcs into states 19 and 20 will include
the exiting arcs of the HMMs associated with pronunciation-network
nodes 5 and 6 because these are the pronunciation network predecessors
of node 7.  Thus, the next step in constructing the whole-utterance
HMM is to modify the individual HMM networks by augmenting the
predecessor lists of their starting states with the exiting arcs of
their pronunciation network predecessors.  This task will be performed
by the function \verb~change_preds~.


        The function \verb~change_preds~ works on a single node in the
pronunciation digraph.  The first argument is an array of all the HMMs
in the pronunciation network.  The second argument is the node to be
changed.  Once we have modified the predecessor lists of the starting
states, we only need the probabilistic digraph in future calculations,
not the entire HMM, so only the digraph part of the HMM is returned.


        The calculation is summarized as follows.  For a given
pronunciation network node $n$, let $i$ be the index of any of the
starting states of the associated HMM and let $\pi_i$ be its starting
probability.  For each node $j$ in the pronunciation network that is a
predecessor of $n$, take each exiting arc $(p,q)$ associated with node
$j$'s HMM and add $(p, q\times \pi_i)$ to the predecessor list of $i$.
For practical reasons, the HMMs use log probabilities, so the
multiplication is actually implemented as an addition.


        The implementation of \verb~change_preds~ uses the array
building function \verb~accum~.  The variable \verb~new_preds~ is an
association list: each starting state $i$ is associated with a list of
new predecessor states.  This list will be prefixed to $i$'s existing
list of predecessor states using the binary operator \verb~op~.  The
function \verb~accum~ can be used because the probabilistic digraph
\verb~pdg~ is represented using an array.
        \begin{haskell}{change_preds}

> change_preds :: Array Int (HmmTsA HmmData)    ->
>                 DigraphNode (HmmTsA HmmData)  ->
>                 ProbDigraphA HmmData

> change_preds  hmma  (HmmTsA is _ pdg, ps)  =  accum op pdg new_preds
>     where
>     new_preds = [(i, [(p, q + pi_i) | j <- ps,
>                                       (p,q) <- stop_states (hmma!j)])
>                  | (i, pi_i) <- is ]
>
>     (x, y) `op` z = (x, z++y)

\end{haskell}


        The function \verb~combineHmms~ determines the starting and
stopping states of the whole-utterance HMM, and uses the function
\verb~change_preds~ to modify each HMM before collapsing the network
of HMMs down to a single HMM.
        \begin{haskell}{combineHmms}

> combineHmms :: PrnNetwork (HmmTsA HmmData) -> HmmTsA HmmData

> combineHmms (PrnN n is ts dg) = HmmTsA is' ts' dga
>       where
>       (hmms, _) = unzip dg
>       hmma      = listArray (1,n) hmms
>
>       is'       = concat [start_states (hmma ! i) | i <- is]
>       ts'       = concat [stop_states  (hmma ! t) | t <- ts]
>
>       n'        = snd (bounds (prb_digraph (hmma ! n)))
>       new_hmms  = concat (map (assocs . change_preds hmma) dg)
>       dga       = array (1,n') new_hmms

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {``{\tt buildHmm}''}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        We now collect the functions defined in the previous
subsections into a single function \verb~buildHmm~ that transforms a
pronunciation network into an hmm.
        \begin{haskell}{buildHmm}

> buildHmm :: HmmNetworkDic -> PrnNetwork (Phone,Int) -> HmmTsA HmmData

> buildHmm hmm_dgs =  combineHmms . reindexHmms . lookupHmms hmm_dgs

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Reading HMMs from a Plain Text File}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        We keep the network descriptions and the observation models
(Chapter~\ref{ch:HmmDensities}) in separate files.  For the network
descriptions, the file is a plain text file containing an association
list of phone symbols and their HMM networks.  The HMM networks are
stored using probabilities for easy reading and modification; the
function \verb~build_hmm_array~ converts the probabilities to the log
domain.  An example of a portion of a HMM network file is shown in
Figure~\ref{fg:hmm.dgs}; the HMMs in this file have the topology shown
in Figure~\ref{fg:phoneHMM2}.
        \begin{figure}
        \input{figs/hmm.dgs}
        \caption[]{An example of a portion of a plain-text HMM network
description file.}
        \label{fg:hmm.dgs}
        \end{figure}


        The following functions are for reading the plain-text HMM
network descriptions.  The function \verb~readElements~ is defined in
the library module \verb~PlainTextIO~ (Chapter~\ref{ch:PlainTextIO}).
The function \verb~readElements~ is polymorphic; it is all we need
since we declared the type \verb~HmmTsL~ to be a member of the class
\verb~Text~.
        \begin{haskell}{readHmms}

> readHmms :: [Char] -> [(Phone, (HmmTsL Int))]
> readHmms = readElements

\end{haskell}


        The HMMs are stored in an array for efficient random access.
        \begin{haskell}{build_hmm_array}

> build_hmm_array :: [(Phone, (HmmTsL HmmState))] ->
>                     Array Phone (HmmTsL HmmState)
> build_hmm_array = array phone_bounds

\end{haskell}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Converting to Log Probabilities}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        Although it is convenient to initialize and study HMMs using
probabilities, the Viterbi algorithm is more efficiently implemented
using log probabilities.  The function \verb~get_log_probs~ converts
the probabilities for all HMMs in an array.
        \begin{haskell}{get_log_probs}

> get_log_probs :: (Ix a) => Array a (HmmTsL b) -> Array a (HmmTsL b)
> get_log_probs = map convert_to_log_probs

\end{haskell}


        The function \verb~convert_to_log_probs~ converts
probabilities to log probabilities for a single HMM.  If a probability
is too small, however, we replace its true log by a floor value.
        \begin{haskell}{convert_to_log_probs}

> convert_to_log_probs :: HmmTsL a -> HmmTsL a

> convert_to_log_probs (HmmTsL n is ts dg) = HmmTsL n is' ts' dg'
>       where
>       is' = mapsnd safelog is
>       ts' = mapsnd safelog ts
>       dg' = mapsnd (mapsnd safelog) dg
>       safelog x = if x <= 1.0e-05 then  -11.5129255 else log x

\end{haskell}


%%%%%%%%%%  End of HmmDigraphs.lhs  %%%%%%%%%%
