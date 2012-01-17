\documentclass[a4paper]{article}
\pagestyle{myheadings}
\usepackage{haskell}
\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Abstract Data Type: Indexed}

(We're basically duplicating GHC's HasBounds class here, but
apparently that isn't implemented quite as one would think from the
documentation.)

Provide an interface like {\tt Array}'s {\tt (!)} operator, or {\tt
(!!)} for lists.  It is up to the user to ensure that the index is
within reasonable bounds.

The {\tt (?)} operator returns the element at the given index, while
{\tt len} returns the ``length''.  Indices should be between $0$ and {\tt
len}.

Two caveats:  The question mark is already used for implicit
parameters, this means it must be followed by whitespace (or numeric
literal?).  And, {\tt len} is not a particular good name for one less
than the length.  ("max" is used elsewhere.  How about "limit", or
"top"? "bound"? "end"?)

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Indexed where

infixl 9 ??

class Indexed a b | a -> b where
    (??) :: a -> Int -> b
    len :: a -> Int

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\end{document}