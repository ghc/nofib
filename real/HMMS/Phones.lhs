
        \begin{haskell}{Phones}

> module Phones( Phone(..), phone_bounds, phone_list ) where
> import Ix--1.3

\end{haskell}


        The symbols for the basic within-word sounds as well as the
between-word sounds are defined as data constructors for the type
\verb~Phone~:
        \begin{haskell}{Phone}

> data Phone =
>       -- within-word sounds --
>       AA  | AE  | AH | AO | AW | AX | AXR | AY |      -- vowels
>       EH  | ER  | EY | IH | IX | IY | OW  | OY |
>       UH  | UW  |
>       L   | EL  | R  | W  | Y  |                      -- semivowels
>       M   | N   | EN | NG |                           -- nasals
>       F   | TH  | S  | SH | HH |                      -- fricatives
>       V   | DH  | Z  | ZH |
>       CH  | JH  |                                     -- affricates
>       P   | T   | K  | B  | D  | G  |                 -- stops
>       DX  |                                           -- flap
>       -- between-word sounds --
>       SIL                                             -- silence
>       deriving (Eq, Ord, Enum, Ix, Read, Show)

\end{haskell}
        The data constructors are the same as a subset of the symbols
in the ``ARPAbet'' phonetic alphabet~\cite[Table 2.1, p.\
24]{RabiJuan93}, except for the ``\verb~SIL~'' constructor which is
represented in the ARPAbet as ``h\#.'' As in \cite[Table II]{Lee90},
\cite[Table 2.1]{RabiJuan93}, and \cite[Table I]{LeeRabiPierWilp90},
the relationships between the phonetic symbols and the sounds they
represent are shown by example in Table~\ref{tb:phones}.
        \begin{table}
        \caption[]{The phonetic symbols and their corresponding sounds
using examples from general North American English.}
        \label{tb:phones}
        \begin{center}
\begin{tabular}{|ll|ll|ll|}
\hline
Symbol & Example & Symbol & Example & Symbol & Example \\
\hline
%%
AA & b\underline{o}b &
ER & b\underline{ir}d &
P  & \underline{p}o\underline{p}  \\
%%
AE & b\underline{a}t &
EY & b\underline{ay} &
R  & \underline{r}ay  \\ 
%%
AH & b\underline{u}tt &
F  & \underline{f}ee &
S  & \underline{s}ee  \\  
%%
AO & b\underline{ough}t &
G  & \underline{g}i\underline{g} &
SH & \underline{sh}e  \\  
%%
AW & b\underline{ou}t &
HH & \underline{h}ay &
T  & \underline{t}o\underline{t}  \\
%%
AX & \underline{a}gain &
IH & b\underline{i}t &
TH & \underline{th}in \\
%%
AXR & din\underline{er} &
IX & ros\underline{e}s &
UH & b\underline{oo}k \\
%%
AY & b\underline{uy} &
IY & b\underline{ee} &
UW & b\underline{oo}t \\
%%
B  & \underline{b}o\underline{b} &
JH & \underline{j}u\underline{dge} &
V  & \underline{v}ee  \\
%%
CH & \underline{ch}ur\underline{ch} &
K  & \underline{k}i\underline{ck} &
W  & \underline{w}ee  \\
%%
D  & \underline{d}i\underline{d} &
L  & \underline{l}ay &
Y  & \underline{y}ou  \\
%%
DH & \underline{th}ey &
M  & \underline{m}o\underline{m} &
Z  & \underline{z}oo  \\
%%
DX & bu\underline{tt}er &
N  & \underline{n}o\underline{n} &
ZH & a\underline{z}ure \\
%%
EL & bott\underline{le} &
NG & si\underline{ng} &
   & \\
%%
EH & b\underline{e}t &
OW & b\underline{oa}t &
   & \\
%%
EN & butt\underline{on} &
OY & b\underline{oy} &
   & \\
%%
 & &   & &   &  \\
\hline
\end{tabular}
        \end{center}
        \end{table}


        The variable \verb~phone_bounds~ is a pair comprised of the
first and last phones.  It can be used for specifying array bounds.
        \begin{haskell}{phone_bounds}

> phone_bounds = (AA, SIL)

\end{haskell}
        The variable \verb~phone_list~ is the complete list of
phones:
        \begin{haskell}{phone_list}

> phone_list = range phone_bounds

\end{haskell}


        The list of phones provided here is sufficient to cover all
English words.  However, for better recognition accuracy, we may want
to expand the symbol set to allow finer distinctions, e.g., {\em
context-dependent phones\/}~\cite[Section 2.6.3]{LeeRabiPierWilp90}.
On the other hand, when building recognizers for small-vocabulary
tasks such as digit or isolated letter recognition, this set could be
reduced.
