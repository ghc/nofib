
        A balanced binary search tree is a data structure enabling
efficient retrieval of data.  For training hidden Markov models on a
large set of training utterances with a large vocabulary, it is
necessary to provide efficient retrieval of word pronunciation models.
This section describes a Haskell implementation of balanced binary
search trees as described in~\cite{BirdWadl88} that is sufficient for
our needs (i.e., we don't implement all of the functions normally
associated with the abstract type).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{The Balanced Binary Search Tree Datatype}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        While the data type and functions are basically those of Bird
\& Wadler~\cite[Chapter 9]{BirdWadl88}, we have extended their search
tree structure by tagging each node in the tree with two values
instead of one: a {\em key\/} and a {\em definition}.  Note that we
only provide a partial implementation of balanced binary search trees;
for example, we have not bothered to implement a ``delete'' function.
        \begin{haskell}{BalBinSTrees}

> module BalBinSTrees(
>       BalBinSTree,        -- don't export the data constructors
>       bbstBuild,
>       bbstInsert,         -- error upon finding duplicate keys
>       bbstInsertQuiet,    -- don't complain about duplicate keys
>       bbstLookUp,
>       bbstMember,
>       bbstDepth,
>       bbstShowKeys,
>       bbstFlatten
>       ) where

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Implementation}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%====================================================================
\subsection*{Representation}
%%====================================================================

        We let the balanced binary search tree data type inherit the
methods of the class ``\verb~Text~'' so that we can easily read and
write such trees from/to plain text files.  The type variable \verb~k~
represents the type of the key and the type variable \verb~b~
represents the type of the data to be retrieved.
        \begin{haskell}{BalBinSTree}

> data BalBinSTree a b =
>       Nil | Node a b (BalBinSTree a b) (BalBinSTree a b)
>       deriving Show{-was:Text-}

\end{haskell}


%%====================================================================
\subsection*{bbstBuild}
%%====================================================================

        The function \verb~bbstBuild~ takes an association list (i.e.,
a list of pairs where the first element of the pair is the ``key'' and
the second element of the pair is the ``definition'') as an argument
and returns a balanced binary search tree.  The key type must belong to
the Haskell class \verb~Ord~ because the function \verb~bbstInsert~
uses the methods of that class.
        \begin{haskell}{bbstBuild}

> bbstBuild     :: (Ord a) => [(a,b)] -> BalBinSTree a b
> bbstBuild     = foldl bbstInsert Nil

\end{haskell}


%%====================================================================
\subsection*{bbstInsert}
%%====================================================================

        The function \verb~bbstInsert~ takes a balanced binary search
tree and an association pair and returns a new balanced binary search
tree which includes the pair, provided that the key is not already
found in the tree.  If the key is already in the tree, an error is
signaled and evaluation is halted.  The definition of
\verb~bbstInsert~ follows the definition of {\em insert\/}
of~\cite[p.\ 255]{BirdWadl88} except for the way we handle duplicate
keys.
        \begin{haskell}{bbstInsert}

> bbstInsert :: (Ord a) => BalBinSTree a b -> (a,b) ->
>                          BalBinSTree a b

> bbstInsert Nil (x,d)  = Node x d Nil Nil

> bbstInsert (Node y e l r) (x,d)
>       | x <  y     =  rebalance (Node y e (bbstInsert l (x,d)) r)
>       | x == y     =  error "duplicate key"
>       | otherwise  =  rebalance (Node y e l (bbstInsert r (x,d)))

\end{haskell}


        The function \verb~bbstInsertQuiet~ is similar to
\verb~bbstInsert~ but doesn't complain about duplicate keys, quietly
returning the original tree.  The definition of this function follows
the definition of {\em insert\/} of~\cite[p.\ 255]{BirdWadl88}.
        \begin{haskell}{bbstInsertQuiet}


> bbstInsertQuiet :: (Ord a) => BalBinSTree a b -> (a,b) ->
>                               BalBinSTree a b

> bbstInsertQuiet Nil (x,d)  = Node x d Nil Nil

> bbstInsertQuiet  t@(Node y e l r)  a@(x,d)
>       | x <  y     =  rebalance (Node y e (bbstInsertQuiet l a) r)
>       | x == y     =  t
>       | otherwise  =  rebalance (Node y e l (bbstInsertQuiet r a))

\end{haskell}


        The function \verb~rebalance~ is used to bring a binary tree
that is slightly out of balance back into balance.  This is the
function {\em rebal\/} of~\cite[p.\ 255]{BirdWadl88}.
        \begin{haskell}{rebalance}

> rebalance    :: BalBinSTree a b -> BalBinSTree a b
> rebalance t  =  case slope t of
>                 2   ->  shift_right t
>                 -2  ->  shift_left t
>                 _   ->  t

\end{haskell}


        The function \verb~slope~ is the function {\em slope\/}
of~\cite[p.\ 253]{BirdWadl88}.
        \begin{haskell}{slope}

> slope                  :: BalBinSTree a b -> Int
> slope  Nil             =  0
> slope (Node _ _ l r)   =  bbstDepth l - bbstDepth r

\end{haskell}


        The function \verb~bbstDepth~ computes the depth of a binary
search tree; it is the function {\em depth\/} of~\cite[p.\
235]{BirdWadl88}.
        \begin{haskell}{bbstDepth}

> bbstDepth                :: BalBinSTree a b -> Int
> bbstDepth  Nil           =  0
> bbstDepth (Node _ _ l r) =  1 + (bbstDepth l `max` bbstDepth r)

\end{haskell}


        The functions \verb~shift_right~ and \verb~shift_left~ are
used to rebalance a tree.  These are the functions {\em shiftr\/} and
{\em shiftl\/} of~\cite[p.\ 255]{BirdWadl88}.
        \begin{haskell}{shift_right}

> shift_right (Node x d l r)
>       | slope l == -1  =  rotate_right (
>                             Node x d (rotate_left l) r)
>
>       | otherwise      =  rotate_right (
>                             Node x d l r)

\end{haskell}
        \fixhaskellspacing\begin{haskell}{shift_left}

> shift_left (Node x d l r)
>       | slope r == 1   =  rotate_left (
>                             Node x d l (rotate_right r))
>
>       | otherwise      =  rotate_left (
>                             Node x d l r)

\end{haskell}


        The two rotation operations are defined as follows.  These are
the functions {\em rotr\/} and {\em rotl\/} of~\cite[p.\
255]{BirdWadl88}.
        \begin{haskell}{rotate_right}

> rotate_right (Node x d (Node y e t1 t2) t3) =
>       Node y e t1 (Node x d t2 t3)

\end{haskell}
        \fixhaskellspacing\begin{haskell}{rotate_left}

> rotate_left  (Node x d t1 (Node y e t2 t3)) =
>       Node y e (Node x d t1 t2) t3

\end{haskell}


%%====================================================================
\subsection*{bbstLookUp}
%%====================================================================

        The function \verb~bbstLookUp~ looks for a given key in the
search tree and returns the definition associated with that key.  We
restrict the key to the class \verb~Ord~ so that it can be compared to
other keys and to the class \verb~Text~ so that an informative error
message can be printed when a key is not found.
        \begin{haskell}{bbstLookUp}

> bbstLookUp :: (Ord a, Show{-was:Text-} a) => BalBinSTree a b -> a -> b

> bbstLookUp Nil x  =  error ("key " ++ shows x " not found in tree")

> bbstLookUp (Node k d l r) x
>       | x <  k  =  bbstLookUp l x
>       | x == k  =  d
>       | x >  k  =  bbstLookUp r x

\end{haskell}


%%====================================================================
\subsection*{bbstMember}
%%====================================================================

        The function \verb~bbstMember~ looks for a given key in the
search tree and returns \verb~True~ if found and \verb~False~ if not.
This is the function {\em member\/} of~\cite[p.\ 246]{BirdWadl88}.
        \begin{haskell}{bbstMember}

> bbstMember :: (Ord a) => BalBinSTree a b -> a -> Bool

> bbstMember Nil _  = False

> bbstMember (Node k d l r) x
>       | x <  k  = bbstMember l x
>       | x == k  = True
>       | x >  k  = bbstMember r x

\end{haskell}


%%====================================================================
\subsection*{bbstShowKeys}
%%====================================================================

        We provide a function for displaying the tree structure along
with the keys for the special case when the key type belongs to the
type class \verb~Text~.  The function uses tab characters to indent
the different levels.
        \begin{haskell}{bbstShowKeys}

> bbstShowKeys :: (Show{-was:Text-} a) => Int -> BalBinSTree a b -> String

> bbstShowKeys ntabs Nil  =  tabs ntabs ++ "NIL\n"

> bbstShowKeys ntabs (Node x _ l r)  =
>       bbstShowKeys (ntabs+1) r  ++
>       tabs ntabs ++ show x ++ "\n" ++
>       bbstShowKeys (ntabs+1) l

> tabs ntabs = take ntabs (repeat '\t')

\end{haskell}


%%======================================================================
\subsection*{bbstFlatten}
%%======================================================================

        The function \verb~bbstFlatten~ returns a list of all of
key-data pairs within the binary search tree.  It is basically the
function {\em labels\/} of~\cite[p.\ 247]{BirdWadl88}.
        \begin{haskell}{bbstFlatten}

> bbstFlatten                :: BalBinSTree a b -> [(a,b)]
> bbstFlatten  Nil           = []
> bbstFlatten (Node k v l r) = bbstFlatten l ++ [(k,v)] ++ bbstFlatten r

\end{haskell}


%%%%%%%%%%  End of BalBinSTrees.lhs  %%%%%%%%%%
