\chapter{Continued Fractions}

\begin{verbatim}
$Log: ContinuedFractions.lhs,v $
Revision 1.2  1999/11/02 16:10:42  simonpj
Haskell 98 changes

Revision 1.1  1996/01/08 20:05:19  partain
Initial revision

\end{verbatim}

> module ContinuedFractions
>  (module Maybe, module QRationals,
>   ContinuedFraction(..), Homography(..), Interval(..), rat2cf,
>   algebraicAlgorithm, algebraicOutput,
>   quadraticAlgorithm, quadraticOutput,
>   cfRat2CFSqrt, decimals, accuracy, cf2Rat, integerFraction)
> where
> import Data.Maybe
> import Data.List( sort )
> import QRationals

\section{Representation of Continued Fractions}

The {\em Continued Fraction} representation of a number $r$ is a list of numbers
\[r = [x_0,~x_1,~x_2,~\ldots~x_i,~]\]
with the following interpretation:
\[r = x_0 + \frac{1}{x_1 + \frac{1}{x_2 +
            \frac{1}{\ddots +\frac{1}{x_i + \frac{1}{ddots}}}}}\]

In this section we will describe all of the representational issues
associated with continued fractions.

We represent continued fractions as a list of @Integer@. The
@Homography@ is described seperately under the Algebraic and Quadratic
Algorithms. An @Interval@ is simply an interval with rational
end-points.

> type ContinuedFraction = [Integer]
> type Homography        = ([Integer],[Integer])
> type Interval          = (QRational,QRational)

To make it easy to negate a continued fraction, we are assuming that
the continued fractions are symmetric in the following sense:
\[-[x_0,\,x1,\,\ldots] = [ -x_0,\,-x_1,\,\ldots ] \]

\subsection{Creating a (Finite) Continued Fraction from a QRational}

The function @rat2cf@ turns a @QRational@ into the equivalent {\bf
Z}-fraction. This is acceptable provided that the invariant that we
choose for the relationship between the leading term of a continued
fraction and its interval is such that:

\[[\lfloor r\rceil - \frac{1}{2},\,\lfloor r\rceil +\frac{1}{2}) \subseteq
@cfBound@\,r\, \]

> rat2cf :: QRational -> ContinuedFraction
> rat2cf q
>  = if qUndefined q then error "rat2cf: undefined" else
>    if qInfinite  q then []                        else
>    rat2cf' q
>    where rat2cf' q = x : if q' == 0 then [] else rat2cf' (1 / q')
>                      where x  = qRound q
>                            q' = q - (x%%1)

\subsection{Enclosing Bounds for Continued Fractions}

The function @cfBound@ gives @QRational@ bounds that enclose the
Euclidean Continued Fraction with leading term @x@. Because @x@ is
known to be @QRational@ -- and hence the denominator of the solution
to the quadratic equation is non-zero -- both of the roots are finite.

We observe the following conventions:
\begin{itemize}
\item The interval $(i,\,s)$ where $s<i$ is the interval
$(-\infty,\,s)\cup(i,\,\infty)$.
\end{itemize}

> cfBound :: QRational -> Interval
> cfBound x
>  = if qInfinite x then (1%%0,   1%%0)    else
>    if abs x < 2   then (-2%%1,  2%%1)    else
>    if x < 0       then (1%%2-x, x+1%%2)  else
>                        (x-1%%2, -x-1%%2)


 > cfBound x
 >  = if qInfinite x then (1%%0, 1%%0) else 
 >    if n > 0       then (c-pm,c+pm)  else
 >                        intersect (c-pm,c+pm)
 >    where x2 = x*x
 >          n  = 3 - x2 -- never 0 because x is QRational!
 >          c  = 4*x / n
 >          pm = r*(x2+1)/n
 >          r  = if x2 < 3 then rootThreePlus else rootThreeMinus
 >          ax = abs x - (1%%2)
 >          intersect (i,s)
 >            = if i >= s  then (max i ax, min s (-ax)) else
 >              if i < ax  then (ax,s) else
 >              if i > -ax then (i,-ax) else
 >                              error "cfBound: multiple intervals"

Two guesses for $\sqrt 3$ are:

> rootThreeMinus = 1732050807 %% 1000000000 :: QRational
> rootThreePlus  = 1732050808 %% 1000000000 :: QRational

These satisfy $@rootThreeMinus@ < \sqrt 3$ and $@rootThreePlus@ > \sqrt 3$.

\subsection{Generating Output Terms of Continued Fractions}

We need a way to generate the next term to be emmitted by the various
algorithms that process the continued fractions; this is accomplished
by the @generateOutput@ function.

If the answer is now known with sufficient accuracy, the function
returns @Just o@ where @o@ is the leading term of the output continued
fraction.  If it is not possible to determine the leading term of the
output continued fraction, @generateOutput@ returns @Nothing@.

> generateOutput
>   :: (Homography -> [QRational] -> Interval)  -> -- Bound
>      (Homography -> [QRational] -> QRational) -> -- Evaluator
>      Homography                               -> -- Homography
>      [QRational]                              -> -- Leading Input Terms
>      Maybe Integer                               -- Possible Output

> generateOutput bound eval homography qs
>  = if not (qInfinite q0 && qInfinite q1) --  && signum (o%%1) == signum o'
>       && acceptable int (o%%1) then Just o else Nothing
>    where int@(q0,q1) = bound homography qs
>          o' = eval homography qs
>          o  = qRound (if abs o' < 2 then o' else
>                       if abs q0 < abs q1 then q0 else q1)

> acceptable :: Interval -> QRational -> Bool
> acceptable int o = cfBound o `encloses` int

\subsection{Interval Operations}

The function @encloses@ tests whether its first interval argument
encloses its second.

> encloses :: Interval -> Interval -> Bool
> (i0,s0) `encloses` (i1,s1)
>  = if i0 > s0 then 
>       if i1 < s1 then i0 <= i1 || s1 <= s0 
>                  else i0 <= i1 && s1 <= s0
>    else i1 < s1 && i0 <= i1 && s1 <= s0

Using $A \cap B = (A^c \cup B^c)^c$ ...

> intersect :: Interval -> Interval -> Interval
> intersect (i,s) (i',s') = (rs,ri) where (ri,rs) = union [(s,i),(s',i')]

> union :: [Interval] -> Interval
> union = result . foldl join [] . sort . finitize
>         where result [int]         = int
>               result [(i,_),(_,s)] = (i,s)
>               result ints          = (0,0)

Assuming that the intervals are finite and ordered....

Then $@i@ \leq @i'@$ and if they're equal $@s@ \leq @s'@$

> join :: [Interval] -> Interval -> [Interval]
> join []             y         = [y]
> join xs@((i,s):iss) y@(i',s') = if s >= s' then xs else
>                                 if s >= i' then (i,s'):iss else y:xs

> finitize
>  = concat . map split
>    where split x@(i,s) = if i < s then [x] else [(-1%%0,s),(i,1%%0)]

> finiteInterval   (i,s) = i < s
> infiniteInterval (i,s) = i > s

\section{The Algebraic Algorithm}

The algebraic algorithm takes a homography and calculates the following:
\[@algebraicAlgorithm@\,\left(\begin{array}[c]{cc} 
n_0 & n_1 \\ d_0 & d_1\end{array}\right)\,x \, = \,
 \frac{n_0x+n_1}{d_0x+d1}\]

Given that $x$ isn't $\bot$, this is well-defined provided that the
determinant of the homography is non-zero. If the determinant {\em is}
zero then the result is independent of the value of $x$.

Because we notice that absorbing a (well-defined and finite) term from
the input continued fraction or emitting a (well-defined and finite)
term of the output continued fraction doesn't change the determinant
of the homography we need only check this once.

> algebraicAlgorithm :: Homography        -> -- Homography
>                       ContinuedFraction -> -- Input continued fraction
>                       ContinuedFraction    -- Output continued fraction

> algebraicAlgorithm homography@([n0,n1],[d0,d1]) xs
>  = if n0*d1 /= n1*d0 then algebraicAlgorithmNoCheck homography xs
>    else if d1 /= 0   then rat2cf (n1%%d1)
>    else if d0 /= 0   then rat2cf (n0%%d0) 
>    else                   error "algebraicAlgorithm: undefined homography"

> algebraicAlgorithmNoCheck :: Homography        -> -- Homography
>                              ContinuedFraction -> -- Input  CF
>                              ContinuedFraction    -- Output CF

> algebraicAlgorithmNoCheck homography@([n0,n1],[d0,d1]) []
>  = rat2cf (n0%%d0)
> algebraicAlgorithmNoCheck homography@([n0,n1],[d0,d1]) xs'@(x:xs)
>  = case algebraicOutput homography (x%%1) of
>      Just o  -> o : algebraicAlgorithmNoCheck ([d0,d1],[n0-d0*o,n1-d1*o]) xs'
>      Nothing ->     algebraicAlgorithmNoCheck ([n0*x+n1,n0],[d0*x+d1,d0]) xs

> algebraicOutput :: Homography ->    -- Homography
>                    QRational  ->    -- Leading Term of Input CF
>                    Maybe Integer    -- Leading Term of Output CF 

> algebraicOutput homography q
>  = if q == 0 then Nothing else a
>    where a = generateOutput algebraicBound algebraicEval homography [q]

\subsection{Bounds on the Output Continued Fraction}

The function @algebraicBound@, given a homography and the first
(@QRational@) term of a continued fraction determines the interval
within which the transformed continued fraction {\em must} lie.

> algebraicBound :: Homography  -> -- Homography
>                   [QRational] -> -- Leading term of input CF
>                   Interval       -- Interval

> algebraicBound homography@([n0,n1],[d0,d1]) [q]
>  = if n0*d1 == n1*d0
>    then (indp, indp) -- bound independent of x
>    else if xi == dcross || xs == dcross -- infinity at endpoint
>         then if xi > xs && (ncross <= xs || xi <= ncross) ||
>                 xi < xs &&  ncross <= xs && xi <= ncross
>              then zint  -- zero in interval
>              else nzint -- zero not in interval
>    else if xi > xs && (d0 == 0 || xi < dcross || dcross < xs) ||
>            xi < xs &&  d0 /= 0 && xi < dcross && dcross < xs
>         then (ys,yi) -- infinity in interval
>         else (yi,ys) -- infinity not in interval
>    where dcross  = (-d1) %% d0
>          ncross  = (-n1) %% n0
>          (xi,xs) = cfBound q
>          yi'     = algebraicEval homography [xi]
>          ys'     = algebraicEval homography [xs]
>          (yi,ys) = (min yi' ys', max yi' ys')
>          indp    = if d0 /= 0 then n0%%d0 else
>                    if d1 /= 0 then n1%%d1 else
>                       error "algebraicBound: denominator 0"
>          zint    = if qInfinite yi then if ys < 0%%1 then (ys,  1%%0)
>                                                      else (-1%%0, ys)
>                                    else if yi < 0%%1 then (yi,  1%%0)
>                                                      else (-1%%0, yi)
>          nzint   = if qInfinite yi then if ys < 0%%1 then (-1%%0, ys)
>                                                      else (ys,  1%%0)
>                                    else if yi < 0%%1 then (-1%%0, yi)
>                                                      else (yi,  1%%0)

\subsection{Evaluating the homography}

The function @algebraicEval@ evaluates the homography applied to a
QRational number.

> algebraicEval :: Homography  -> -- Homography
>                  [QRational] -> -- Input QRational
>                  QRational      -- Output QRational

> algebraicEval ([n0,n1],[d0,d1]) [x] = (n0*xn + n1*xd) %% (d0*xn + d1*xd)
>                                        where xn = qNumerator   x
>                                              xd = qDenominator x

\section{Quadratic Functions}

In a similar fashion we can define the Quadratic Algorithm. This
calculates the following sum:

\[@quadraticAlgorithm@~
\left(\begin{array}[c]{cccc}n_0 & n_1 & n_2 & n_3\\d_0 & d_1 & d_2 & d_3
\end{array}\right)~x~y
= \frac{n_0xy + n_1y + n_2x + n_3}{d_0xy +d_1y +d_2x + d_3}
\]

I could believe that the @quadraticAlgorithm@ might not work correctly
for finite continued fractions, as I haven't checked the conditions on
the homography for definedness. On the positive side, there shouldn't
be any finite continued fractions anyway.

> quadraticAlgorithm :: Homography         -> -- Homography
>                       ContinuedFraction  -> -- First Input CF
>                       ContinuedFraction  -> -- Second Input CF
>                       ContinuedFraction     -- Output CF

> quadraticAlgorithm homography@([n0,n1,n2,n3],[d0,d1,d2,d3]) []  []
>  = rat2cf (n0 %% d0)
> quadraticAlgorithm homography@([n0,n1,n2,n3],[d0,d1,d2,d3]) []  ys'@(y:ys)
>  = if aaDet homography' == 0
>    then quadraticAlgorithm (qaInRight homography y) [] ys
>    else algebraicAlgorithmNoCheck homography' ys'
>    where homography' = ([n0,n2],[d0,d2])
> quadraticAlgorithm homography@([n0,n1,n2,n3],[d0,d1,d2,d3]) xs'@(x:xs) []
>  = if aaDet homography' == 0
>    then quadraticAlgorithm (qaInLeft homography x) xs []
>    else algebraicAlgorithmNoCheck homography' xs'
>    where homography' = ([n0,n1],[d0,d1])
> quadraticAlgorithm homography xs'@(x:xs) ys'@(y:ys)
>  = case quadraticOutput homography [x%%1,y%%1] of
>      Just o  -> o : quadraticAlgorithm (qaOut homography o)   xs' ys'
>      Nothing ->     quadraticAlgorithm (qaIn  homography x y) xs  ys

> quadraticOutput :: Homography   -> -- Homography
>                    [QRational]  -> -- Leading Term of Input CF
>                    Maybe Integer   -- Leading Term of Output CF 
> quadraticOutput = generateOutput quadraticBound quadraticEval

> qaIn :: Homography -> Integer -> Integer -> Homography
> qaIn ([n0,n1,n2,n3],[d0,d1,d2,d3]) x y
>  = ([(n0*x+n1)*y+n2*x+n3, n0*y+n2, n0*x+n1, n0],
>     [(d0*x+d1)*y+d2*x+d3, d0*y+d2, d0*x+d1, d0])

> qaOut :: Homography -> Integer -> Homography
> qaOut ([n0,n1,n2,n3],[d0,d1,d2,d3]) o
>  = ([d0,d1,d2,d3], [n0-d0*o, n1-d1*o, n2-d2*o, n3-d3*o])

> qaInLeft :: Homography -> Integer -> Homography
> qaInLeft ([n0,n1,n2,n3],[d0,d1,d2,d3]) x
>  = ([n0*x+n1, n0, n2*x+n3, n2], [d0*x+d1, d0, d2*x+d3, d2])

> qaInRight :: Homography -> Integer -> Homography
> qaInRight ([n0,n1,n2,n3],[d0,d1,d2,d3]) y
>  = ([n0*y+n2, n1*y+n3, n0, n1], [d0*y+d2, d1*y+d3, d0, d1])

> aaDet :: Homography -> Integer
> aaDet ([n0,n1],[d0,d1]) = n0*d1 - n1*d0

[Worry that it would be quicker to do Integer arithmetic in qaEval.]

> quadraticEval :: Homography       -> -- homography
>                  [QRational]      -> -- QRational
>                  QRational                -- result

> quadraticEval (ns,ds) [x, y]
>  = ((n0*x+n1)*y + (n2*x+n3)) / ((d0*x+d1)*y + (d2*x+d3))
>    where [n0,n1,n2,n3] = map (%% 1) ns
>          [d0,d1,d2,d3] = map (%% 1) ds

> quadraticBound :: Homography  -> -- homography
>                   [QRational] -> -- terms of CFs
>                   Interval       -- interval

> quadraticBound ([n0,n1,n2,n3],[d0,d1,d2,d3]) [x, y]
>  = union ints
>    where (xi,xs) = cfBound x
>          (yi,ys) = cfBound y
>          ints    = [algebraicBound (aax x) [y] | x <- [xi,xs]] ++
>                    [algebraicBound (aay y) [x] | y <- [yi,ys]]
>          aax x   = ([n0*xn+n1*xd,n2*xn+n3*xd],[d0*xn+d1*xd,d2*xn+d3*xd])
>                    where xn = qNumerator x
>                          xd = qDenominator x
>          aay y   = ([n0*yn+n2*yd,n1*yn+n3*yd],[d0*yn+d2*yd,d1*yn+d3*yd])
>                    where yn = qNumerator y
>                          yd = qDenominator y

\section{Square Roots}

\subsection{Rational Square Roots}

Notice that there is no way we can test for rationalness of these
continued fractions.

> cfRat2CFSqrt :: QRational -> ContinuedFraction
> cfRat2CFSqrt q
>  = map snd (next ([2*d*z,d,n-d*z*z,0], z)) 
>    where n = qNumerator q
>          d = qDenominator q
>          s = intSquareRoot (n*d)
>          z = qRound (s%%d)
>          t = s - z*d
>          next :: ([Integer],Integer) -> [([Integer],Integer)]
>          next (s,z) = if d0 == 0
>                       then [(s,z)]
>                       else (s,z): next ([n0',n1',d0',d1'],z')
>                       where [n0,n1,d0,d1] = s
>                             n0' = d0*z'+d1
>                             n1' = d0
>                             d0' = n0*z'+n1-z'*n0'
>                             d1' = n0-z'*d0
>                             z'  = qRound ((n0+t) %% d0)

\subsection{Integer Square Roots}

The function @intSquareRoot@ returns a non-negative integer $n$, such
that $n = \lfloor\sqrt{@x@}\, \rceil$.

> intSquareRoot :: Integer -> Integer
> intSquareRoot x = until satisfy (improve x) x
>                   where satisfy y    = y2+y >= x && y2-y < x
>                                        where y2 = y*y
>                         improve x y  = (y*y+x) `ddiv` (2*y)
>                         ddiv x y     = if (r <= y `div` 2) then q else q+1
>                                        where (q,r) = divMod x y

> root2 = cfRat2CFSqrt (2%%1)
> root3 = cfRat2CFSqrt (3%%1)
> test0 :: ContinuedFraction
> test0 = f 1 where f n = 0: 10^n: f (n+1)

For debugging.

\section{Conversion of Continued Fractions to QRationals}

I've chosen to print out 10 decimal places; you can add more (or less)
by changing @decimals@.

> decimals = 10 :: Int

> accuracy = 1 %% (10^(toInteger decimals)) :: QRational

To convert a continued fraction to a rational within the given
accuracy, we use a variant of the Algebraic Algorithm.

> cf2Rat :: ContinuedFraction -> QRational
> cf2Rat = outputAlgorithm ([1,0],[0,1])

> outputAlgorithm :: Homography        -> -- Homography
>                    ContinuedFraction -> -- Input CF
>                    QRational            -- Final Approximation

> outputAlgorithm homography@([n0,n1],[d0,d1]) [] = n0 %% d0
> outputAlgorithm homography@([n0,n1],[d0,d1]) xs'@(x:xs)
>  = if i <= s && s-i < accuracy
>    then algebraicEval homography [x%%1]
>    else outputAlgorithm ([n0*x+n1,n0],[d0*x+d1,d0]) xs
>    where (i,s) = algebraicBound homography [x%%1]


> integerFraction :: ContinuedFraction           -> -- Input CF
>                    (Integer,ContinuedFraction)    -- Answer
> integerFraction x = (o, algebraicAlgorithm h' f)
>                     where (h,f) = intFrac (([1,0],[0,1]),x)
>                           o     = qRound (algebraicEval h [head f%%1])
>                           h'    = ([n0-d0*o,n1-d1*o],[d0,d1])
>                                   where ([n0,n1],[d0,d1]) = h

> intFrac :: (Homography,ContinuedFraction) -> (Homography,ContinuedFraction)
> intFrac st@(_,[_]) = st
> intFrac st@(homography@([n0,n1],[d0,d1]),(x:xs))
>  = if i <= s && s-i < 1 %% 10
>    then st
>    else intFrac (([n0*x+n1,n0],[d0*x+d1,d0]), xs)
>    where (i,s) = algebraicBound homography [x%%1]



