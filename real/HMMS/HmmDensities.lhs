
        In Chapter~\ref{ch:HmmDigraphs} we were concerned only with
the HMM state transition structure.  In this chapter we are concerned
with developing and using statistical models for the observation
vectors.
        \begin{haskell}{HmmDensities}

> module HmmDensities(
>       module MathTypes, module Phones,
>       GaussianComponent, TiedMixture(..), TmTable,
>       LogDensityTable,
>       eval_log_densities, readMixtures, readMixture, extern_to_intern
>       ) where

> import Native         -- hbc library modules

> import Lists          -- general library modules
> import MathTypes
> import MaybeStateT

> import Phones         -- application-specific modules
> import HmmConstants
> import Array--1.3

#if __HASKELL1__ < 5
#define amap map
#else
#define amap fmap
#endif


\end{haskell}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Multivariate Gaussian Densities}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        Let $x$ be a vector of dimension $N$ with real-valued
components, i.e., $x \in \reals^N$.  The formula for a multivariate
Gaussian probability density function having mean vector $\mu \in
\reals^N$ and covariance matrix $\Sigma \in \reals^{N \times N}$ is
        \begin{equation}
        \scriptN(x \verticalbar \mu, \Sigma)
        = \frac{1}{ (2\pi)^{N/2}
          (\det \Sigma)^{1/2} } \exp \{ -\frac{1}{2} (x - \mu)^{T}
          \Sigma^{-1} (x - \mu) \}.
        \label{eq:gauss-gen}
        \end{equation}
        The superscript $T$ denotes the matrix/vector transpose;
vectors without the superscript $T$ are taken to be column vectors.


        If the covariance matrix is diagonal, i.e., $\Sigma =
\mbox{diag}\{\sigma_1^2,\sigma_2^2,\ldots,\sigma_N^2\}$, and if we
take $x = (x_1, x_2, \ldots, x_N)$ and $\mu = (\mu_1, \mu_2, \ldots,
\mu_N)$, then (\ref{eq:gauss-gen}) can be simplified to
        \begin{equation}
        \scriptN(x \verticalbar \mu, \Sigma) = 
          \frac{1}{ (2\pi)^{N/2}
                    \prod_{j=1}^N \sigma_j}
          \exp \{ -\frac{1}{2}
                   \sum_{j=1}^N (x_j - \mu_j)^2 / \sigma_j^2 \} .
        \label{eq:gauss-diag}
        \end{equation}


        We will assume diagonal covariance matrices for the rest of
this chapter.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Mixture Densities}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        Given $M$ density functions $f_1, f_2, \ldots, f_M$ defined
over $\reals^N$ and $M$
nonnegative real numbers $c_1, c_2, \ldots, c_M$ for which
        \[
        \sum_{i=1}^M c_i \ = \ 1,
        \]
        we can define a new density function $g$ over $\reals^N$ by
the equation
        \[
        g = \sum_{i=1}^M c_i f_i.
        \]
        The function $g$ is called a {\em mixture density function}.
The $c_i$'s are the {\em mixture coefficients\/} or the {\em component
probabilities}, and the $f_i$'s are the {\em component densities}.


        An excellent introduction to the theory of mixture
distributions is provided in~\cite{EverHand81}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Gaussian Mixture Densities}
\label{sc:gms}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        A {\em Gaussian mixture density\/} is a mixture density for
which each component density is Gaussian.  Thus, a Gaussian mixture
density has the form
        \begin{equation}
        g(x) = \sum^M_{i=1} c_i \scriptN(x \verticalbar \mu_i,\Sigma_i)
        \label{eq:mixG-gen}
        \end{equation} where the $i$th component density has mean
$\mu_i$ and covariance matrix $\Sigma_i$.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {Representation}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        Recall that we are assuming that the covariance matrix is
diagonal; hence, it can be represented as a vector.  The parameters of
a Gaussian mixture can be stored in a list of triples.  The first
component of each triple is the mixture coefficient, the second
component is the mean vector, and the third component is the variance
vector.
        \begin{haskell}{GaussianComponent}

> type GaussianComponent = (Float, Vector, Vector)

\end{haskell}
        \fixhaskellspacing\begin{haskell}{GaussianMixture}

> type GaussianMixture   = [GaussianComponent]

\end{haskell}
        For example, a 3-component mixture density would be
represented by the list
        \[
        \startlist (c_1, \mu_1, \Sigma_1),\ 
                   (c_2, \mu_2, \Sigma_2),\ 
                   (c_3, \mu_3, \Sigma_3) \stoplist
        \]


        This is how we will store the mixture parameters in external
files.  However, to make the evaluation of a mixture density more
efficient, it pays to modify the {\em internal\/} representation
somewhat.


        Using a subscript $i$ to index the mixture components and
another subscript $j$ to index the vector components, we can replace
each $\scriptN(x \verticalbar \mu_i, \Sigma_i)$ in (\ref{eq:mixG-gen})
by its definition (\ref{eq:gauss-diag}) to get
        \begin{equation}
           g(x) = \sum^M_{i=1}
             \frac{c_i}{  (2\pi)^{N/2} \prod_{j=1}^N \sigma_{i,j} }
            \exp \{ -\frac{1}{2} \sum_{j=1}^N (x_j - \mu_{i,j})^2 /
            \sigma_{i,j}^2 \}.
        \end{equation}
        Denote the ``normalized'' mixture coefficient by $c'_i$:
        \begin{equation}
          c'_i = \frac{c_i}{  (2\pi)^{N/2} \prod_{j=1}^N \sigma_{i,j} }.
        \label{eq:normalized-coefficient}
        \end{equation}
        Then
        \begin{eqnarray}
          g(x) & = & \sum^M_{i=1} c'_i \exp \{ -\frac{1}{2}
                     \sum^N_{j=1} (x_j - \mu_{i,j})^2 / \sigma_{i,j}^2 \}\\
               & = & \sum^M_{i=1} c'_i \exp \{ 
                    - \sum^N_{j=1} (x_j - \mu_{i,j})^2 / (2\sigma_{i,j}^2) \}.
        \label{eq:mixture-final-form}
        \end{eqnarray}


        The internal representation has the same structure as the
external representation, i.e., a list of triples of type
\verb~GaussianComponent~, except the mixture coefficient is replaced
by the normalized mixture coefficient and each of the variances is
multiplied by two.  For example, a 3-component mixture density would
be represented internally by the list
        \[
        \startlist (c'_1, \mu_1, \Sigma'_1),\ 
                   (c'_2, \mu_2, \Sigma'_2),\ 
                   (c'_3, \mu_3, \Sigma'_3) \stoplist
        \]
        where
        \[
        \Sigma_i = \mbox{diag}\{\sigma_{i,1}^2, \sigma_{i,2}^2,
                                \ldots, \sigma_{i,N}^2 \}
        \ \Rightarrow\ 
        \Sigma'_i = \mbox{diag}\{2\sigma_{i,1}^2, 2\sigma_{i,2}^2,
                                \ldots, 2\sigma_{i,N}^2 \}
        \]


        We need to know the dimension of the observation vectors to
calculate the Gaussian normalization coefficient efficiently.  We
could do the calculation without explicit knowledge of the observation
dimension by putting a $\sqrt{2\pi}$ term inside the multiplication
over the $\sigma_{i,j}$'s (see (\ref{eq:normalized-coefficient})), but
this increases the amount of computation.  Since we need the
observation dimension for other functions, we assume that we have
access to it via the module \verb~HmmConstants~.
        \begin{haskell}{gaussian_factor}

> gaussian_factor = sqrt (2.0*pi) ^ observation_dimen :: Double

\end{haskell}
        The normalizing calculation is performed using double
precision arithmetic.  The expression \verb~fromRational.toRational~
is a Haskell idiom for conversion of floating point types.
        \begin{haskell}{extern_to_intern}

> extern_to_intern :: GaussianMixture -> GaussianMixture
> extern_to_intern = map extern_to_intern'

> extern_to_intern' :: GaussianComponent -> GaussianComponent
> extern_to_intern' (c, mu, vars) =
>       let
>         vars_d   = map (fromRational.toRational) vars :: [Double]
>         t        = gaussian_factor * sqrt (product vars_d)
>         denom    = (fromRational.toRational) t :: Float
>       in
>         (c/denom, mu, map (2*) vars)

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection {Evaluation}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        In the Viterbi alignment algorithm~(Chapter~\ref{ch:Viterbi}),
we are actually going to do our calculations on a logarithmic scale.
Thus, the function we really want to compute is
        \begin{equation}
          \ln g(x) = \ln\ \sum^M_{i=1} c'_i \exp \{ 
                - \sum^N_{j=1} (x_j - \mu_{i,j})^2 / (2\sigma_{i,j}^2) \}.
        \label{eq:log-mixture-final-form}
        \end{equation}


        The function for evaluating (\ref{eq:log-mixture-final-form})
with parameters \verb~igm~ (for ``internal Gaussian mixture'') at the
point \verb~x~ can be defined as follows:
        \begin{haskell}{eval_log_mixture}

> eval_log_mixture :: Vector -> GaussianMixture -> Float

> eval_log_mixture  x  igm  =
>       density_log (sum [c' * exp( eval_exponent x mu vars' )
>                            | (c', mu, vars') <- igm])

\end{haskell}
        \fixhaskellspacing\begin{haskell}{eval_exponent}

> eval_exponent (x:rxs) (u:rus) (v:rvs) =
>       let  t = x - u
>       in   (eval_exponent rxs rus rvs) - (t*t)/v

> eval_exponent [] [] [] = 0.0

\end{haskell}


        The use of the function \verb~density_log~ is an attempt to
avoid an unnecessary logarithm calculation if the density value is too
small.
        \begin{haskell}{density_log}

> density_log x = if x <= density_threshold
>                 then smallest_log_density
>                 else log x

\end{haskell}
        \fixhaskellspacing\begin{haskell}{density_threshold}

> density_threshold = 0.1023 ^ observation_dimen :: Float

\end{haskell}
        \fixhaskellspacing\begin{haskell}{smallest_log_density}

> smallest_log_density = log density_threshold

\end{haskell}
        We now explain how we picked the formula for the density
threshold.  Equation (\ref{eq:gauss-diag}) is the probability density
function for $N$ mutually independent Gaussian random variables.  The
probability that an individual zero-mean unit variance Gaussian random
variable takes on an absolute value exceeding 1.65 is about 0.10.  The
value of the density function at this point is about 0.1023.  Since
the Gaussian density function is unimodal, if all $N$ components had
taken values exceeding 1.65 in absolute value, the value of the
density function at such a point would have to be smaller than
$0.1023^N$.  But the probability of this happing is not greater than
$0.10^N$.  So, if our density is this small, it is quite unlikely that
the observed vector came from this particular mixture, so we don't
bother to compute the logarithm precisely.\footnote{Of course, there
are other ways that the density function can be that small; this
calculation is just an engineering approximation.}  Note that, like
the Gaussian normalizing factor, these constants depend on the
dimension of the observation vector, which is defined in the module
\verb~HmmConstants~; see Chapter~\ref{ch:Constants}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {The Log-Density Table}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        The states of each HMM are indexed by integers.
        \begin{verbatim}

> type HmmState = Int

\end{verbatim}


        Each state of a phonetic HMM either has its own mixture
density or it is {\em tied\/} to the density of another state,
possibly belonging to another HMM.
        \begin{haskell}{TiedMixture}

> data TiedMixture = Gm GaussianMixture | Tie Phone HmmState

\end{haskell}


        The tied mixtures are stored in a two-dimensional table that
is implemented as an array of arrays in order to save storage space
when building tables for HMMs with different numbers of states.
        \begin{haskell}{TmTable}

> type TmTable = Array Phone (Array HmmState TiedMixture)

\end{haskell}


        The log-density values are stored in a table that has the same
structure as the mixture parameter table.
        \begin{haskell}{LogDensityTable}

> type LogDensityTable = Array Phone (Array HmmState Float)

\end{haskell}


        The function \verb~eval_log_densities~ computes a table of
log-density values from an observation vector and a tied mixture
table.  In other words, this function evaluates the log-densities for
all of the states of all of the HMMs, placing the values in an array
for efficient retrieval.
        \begin{haskell}{eval_log_densities}

> eval_log_densities :: TmTable -> Vector -> LogDensityTable

> eval_log_densities tmt x = ldt
>       where ldt = amap (amap eval_tied_mixture) tmt
>             eval_tied_mixture (Gm gm)   = eval_log_mixture x gm
>             eval_tied_mixture (Tie p k) = ldt!p!k

\end{haskell}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section {Reading Gaussian Mixture Densities from Binary Files}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        The function readMixtures reads all of the mixtures in a
binary file that is assumed to contain only mixture data.
        \begin{haskell}{readMixtures}

> readMixtures          :: Bytes -> [GaussianMixture]

> readMixtures bs       =
>       case readMixture bs of
>       Nothing         -> if null bs
>                          then []
>                          else error "readMixtures: left-over bytes"
>       Just (m, bs')   -> m : readMixtures bs'

\end{haskell}


        The function \verb~readMixture~ reads the parameters for one
mixture density.  First it reads the number of component densities in
the mixture, then it reads each of them in.  This function and the
next two that follow are implemented using the Maybe State Transformer
(Chapter~\ref{ch:MaybeStateT}).
        \begin{haskell}{readMixture}

> readMixture :: MST Bytes GaussianMixture

> readMixture =
>       readBytes                     `bindMST` \ num_components ->
>       readGaussians num_components  `bindMST` \ gs ->
>       returnMST gs

\end{haskell}


        The first argument to the function \verb~readGaussians~ is the
number of component Gaussian densities in the mixture.
        \begin{haskell}{readGaussians}

> readGaussians         :: Int -> MST Bytes GaussianMixture

> readGaussians  0      = returnMST []

> readGaussians m       = readGaussian        `bindMST` \ g ->
>                         readGaussians (m-1) `bindMST` \ gs ->
>                         returnMST (g:gs)

\end{haskell}


        The function \verb~readGaussian~ reads the mixture
coefficient, the mean, and the variance vector for a multivariate
Gaussian density.
        \begin{haskell}{readGaussian}

> readGaussian      :: MST Bytes GaussianComponent

> readGaussian =
>       readBytes                         `bindMST` \ c ->
>       listReadBytes observation_dimen   `bindMST` \ m ->
>       listReadBytes observation_dimen   `bindMST` \ v ->
>       returnMST (c, m, v)

\end{haskell}


%%%%%%%%%%  End of HmmDensities.lhs  %%%%%%%%%%
