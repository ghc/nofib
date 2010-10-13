\chapter{Implementation of @RealReal@'s}

\begin{verbatim}
$Log: RealReals.lhs,v $
Revision 1.3  1999/11/02 16:10:42  simonpj
Haskell 98 changes

Revision 1.2  1996/07/25 21:30:49  partain
Bulk of final changes for 2.01

Revision 1.1  1996/01/08 20:05:19  partain
Initial revision

\end{verbatim}

> module RealReals (RealReal) where
> import Transcendentals
> import Data.Ratio
> import Data.List( genericLength )
> import Numeric( readSigned )

> data RealReal = RealInt Integer           |
>                 RealRat QRational         |
>                 RealCF  ContinuedFraction

\section{@RealReal@ is an instance of @Eq@}

It should be remembered that computable real numbers do not permit 
computable comparisons, but never mind!

> instance Eq RealReal where
>   (==) = realRealComp (==)
>   (/=) = realRealComp (/=)

The actual comparison is carried out by subtracting @y@ from @x@ and
testing the result at our current accuracy.

> realRealComp :: (QRational -> QRational -> Bool) -> -- Operator
>                 RealReal                         -> -- First Argument
>                 RealReal                         -> -- Second Argument
>                 Bool                                -- Result
> realRealComp op x y = op (makeRational (x-y)) 0

\section{@RealReal@ is an instance of @Ord@}

Once more, these operations are not computable.

> instance Ord RealReal where
>   (<=) = realRealComp (<=)
>   (<)  = realRealComp (<)
>   (>=) = realRealComp (>=)
>   (>)  = realRealComp (>)

\section{@RealReal@ is an instance of @Enum@}

We make @RealReal@ an instance of @Enum@.

> instance Enum RealReal where
>   enumFrom         = iterate (+1)
>   enumFromThen n m = iterate (+(m-n)) n

\section{@RealReal@ is an instance of @Num@}

In this section we come to operations that {\em are} computable.

> instance Num RealReal where
>   x + y         = realRealAdd x y
>   negate x      = realRealNegate x
>   x * y         = realRealMultiply x y
>   abs x         = realRealAbs x
>   signum x      = realRealSignum x
>   fromInteger x = RealInt x

\subsection{Addition}

> realRealAdd :: RealReal -> RealReal -> RealReal
> realRealAdd (RealInt x) (RealInt y) = RealInt (x+y)
> realRealAdd (RealInt x) (RealRat y) = wrapRat ((x%%1)+y)
> realRealAdd (RealInt x) (RealCF  y) = RealCF (algebraicAlgorithm h y)
>                                       where h  = ([1,x],[0,1])
> realRealAdd (RealRat x) (RealInt y) = wrapRat (x+(y%%1))
> realRealAdd (RealRat x) (RealRat y) = wrapRat (x+y)
> realRealAdd (RealRat x) (RealCF  y) = RealCF (algebraicAlgorithm h y)
>                                       where nx = qNumerator   x
>                                             dx = qDenominator x
>                                             h  = ([dx,nx],[0,dx])
> realRealAdd (RealCF  x) (RealInt y) = RealCF (algebraicAlgorithm h x)
>                                       where h  = ([1,y],[0,1])
> realRealAdd (RealCF  x) (RealRat y) = RealCF (algebraicAlgorithm h x)
>                                       where ny = qNumerator   y
>                                             dy = qDenominator y
>                                             h  = ([dy,ny],[0,dy])
> realRealAdd (RealCF  x) (RealCF  y) = RealCF (quadraticAlgorithm h x y)
>                                       where h  = ([0,1,1,0],[0,0,0,1])

The @wrapRat@ function ensures that a rational number that is in fact
an integer, is so treated.

> wrapRat :: QRational -> RealReal
> wrapRat x = if qDenominator x == 1 then RealInt (qNumerator x) else
>                                         RealRat x

\subsection{Negation}

Here we are using the symmetry of the continued fraction
representation, so that we can simply map @negate@ over the continued
fraction.

> realRealNegate :: RealReal -> RealReal
> realRealNegate (RealInt x) = RealInt (negate x)
> realRealNegate (RealRat x) = RealRat (negate x)
> realRealNegate (RealCF  x) = RealCF  (map negate x)

\subsection{Multiplication}

> realRealMultiply :: RealReal -> RealReal -> RealReal
> realRealMultiply (RealInt x) (RealInt y) = RealInt (x*y)
> realRealMultiply (RealInt x) (RealRat y) = wrapRat ((x%%1)*y)
> realRealMultiply (RealInt x) (RealCF  y) = RealCF (algebraicAlgorithm h y)
>                                            where h  = ([x,0],[0,1])
> realRealMultiply (RealRat x) (RealInt y) = wrapRat (x*(y%%1))
> realRealMultiply (RealRat x) (RealRat y) = wrapRat (x*y)
> realRealMultiply (RealRat x) (RealCF  y) = RealCF (algebraicAlgorithm h y)
>                                            where nx = qNumerator   x
>                                                  dx = qDenominator x
>                                                  h  = ([nx,0],[0,dx])
> realRealMultiply (RealCF  x) (RealInt y) = RealCF (algebraicAlgorithm h x)
>                                            where h  = ([y,0],[0,1])
> realRealMultiply (RealCF  x) (RealRat y) = RealCF (algebraicAlgorithm h x)
>                                            where ny = qNumerator   y
>                                                  dy = qDenominator y
>                                                  h  = ([ny,0],[0,dy])
> realRealMultiply (RealCF  x) (RealCF  y) = RealCF (quadraticAlgorithm h x y)
>                                            where h  = ([1,0,0,0],[0,0,0,1])

\subsection{Abs}

We have a problem here. If @signum x == -1@ then we negate the
continued fraction; otherwise we leave the continued fraction
alone.

> realRealAbs :: RealReal -> RealReal
> realRealAbs x = if realRealSignum x < 0 then realRealNegate x else x

\subsection{Signum}

This isn't computable either, so again {\it caveat emptor}.

> realRealSignum :: RealReal -> RealReal
> realRealSignum x = wrapRat (signum (makeRational x))

\section{@RealReal@ is an instance of @Real@}

This is yet another non-computable operation that is fudged.

> instance Real RealReal where
>   toRational rx = qNumerator x % qDenominator x
>                   where x = makeRational rx

\section{@RealReal@ is an instance of @Fractional@}

Fortunately these operations are computable.

> instance Fractional RealReal where
>   x / y          = realRealDivide x y
>   fromRational x = wrapRat (numerator x %% denominator x)

> realRealDivide :: RealReal -> RealReal -> RealReal
> realRealDivide (RealInt x) (RealInt y) = wrapRat (x%%y)
> realRealDivide (RealInt x) (RealRat y) = wrapRat ((x%%1)/y)
> realRealDivide (RealInt x) (RealCF  y) = RealCF (algebraicAlgorithm h y)
>                                          where h  = ([0,x],[1,0])
> realRealDivide (RealRat x) (RealInt y) = wrapRat (x/(y%%1))
> realRealDivide (RealRat x) (RealRat y) = wrapRat (x/y)
> realRealDivide (RealRat x) (RealCF  y) = RealCF (algebraicAlgorithm h y)
>                                          where nx = qNumerator   x
>                                                dx = qDenominator x
>                                                h  = ([0,nx],[dx,0])
> realRealDivide (RealCF  x) (RealInt y) = RealCF (algebraicAlgorithm h x)
>                                          where h  = ([1,0],[0,y])
> realRealDivide (RealCF  x) (RealRat y) = RealCF (algebraicAlgorithm h x)
>                                          where ny = qNumerator   y
>                                                dy = qDenominator y
>                                                h  = ([dy,0],[0,ny])
> realRealDivide (RealCF  x) (RealCF  y) = RealCF (quadraticAlgorithm h x y)
>                                          where h  = ([0,0,1,0],[0,1,0,0])

\section{@RealReal@ is an instance of @Floating@}

> instance Floating RealReal where
>   pi    = realRealPi
>   exp   = realRealExp
>   log   = realRealLog
>   sqrt  = realRealSqrt
>   sin   = realRealSin
>   cos   = realRealCos
>   tan   = realRealTan
>   asin  = realRealAsin
>   acos  = realRealAcos
>   atan  = realRealAtan
>   sinh  = realRealSinh
>   cosh  = realRealCosh
>   tanh  = realRealTanh
>   asinh = realRealAsinh
>   acosh = realRealAcosh
>   atanh = realRealAtanh

Here we notice that $\pi$ is a computable real number.

> realRealPi :: RealReal
> realRealPi = 4 * (RealCF atan1)

\subsection{logarithms and exponentials}

> realRealExp :: RealReal -> RealReal
> realRealExp (RealInt x) = RealCF exp1 ^^ x
> realRealExp (RealRat x) = RealCF exp1 ^^ i * RealCF (expQ (x-(i%%1)))
>                           where i = qRound x
> realRealExp (RealCF  x) = RealCF exp1 ^^ i * RealCF (expR x')
>                           where (i,x') = integerFraction x

> realRealLog :: RealReal -> RealReal
> realRealLog (RealInt x)
>  = if x==1 then RealInt 0 else RealCF log10 * RealInt i + RealCF (logQ x')
>    where i  = genericLength (show x):: Integer
>          x' = x %% (10^i)
> realRealLog (RealRat x)
>  = RealCF log10 * RealInt i + RealCF (logQ x')
>    where i   = if x > 1 then f x else negate (f (1/x))
>          f  :: QRational -> Integer
>          f x = genericLength (show (qRound x + 1))
>          x'  = x / ((10%%1)^^i)
> realRealLog (RealCF  x)
>  = RealCF log10 * RealInt i + RealCF (logR x')
>    where i   = if head x /= 0 then f x else negate (f (tail x))
>          f  :: ContinuedFraction -> Integer
>          f x = genericLength (show (i + 1)) where (i,_) = integerFraction x
>          m   = (1%%10)^^i
>          x'  = algebraicAlgorithm ([qNumerator m,0],[0,qDenominator m]) x

> realRealSqrt :: RealReal -> RealReal
> realRealSqrt    (RealInt x) = RealCF (cfRat2CFSqrt (x%%1))
> realRealSqrt    (RealRat x) = RealCF (cfRat2CFSqrt x)
> realRealSqrt rx@(RealCF  _) = exp (log rx / 2)

\subsection{Trigonometry}

The basic idea for @sin@ and @cos@ is to use the following formul\ae .
\[\sin(x) = \frac{2 \tan(x/2)}{1+\tan^2(x/2)} \mbox{ and }
  \cos(x) = \frac{1-\tan^2(x/2)}{1+\tan^2(x/2)}\]

Unfortunately, we observe that there is a problem at the points
$(2k+1)\pi / 2$ since the @tan@ function switches from $+\infty$ to
$-\infty$. To cure this we observe the following identities that are
based on the value of the cotangent function:
\[\sin(x) = \frac{2 \cot(x/2)}{1+\cot^2(x/2)} \mbox{ and }
  \cos(x) = \frac{\cot^2(x/2)-1}{\cot^2(x/2)+1}\]

> realRealSin :: RealReal -> RealReal
> realRealSin rx@(RealCF _)
>  = 2*c/(1+c*c)
>    where (RealCF x2) = rx/2
>          cotx2       = cotR x2
>          tanx2       = tanR x2
>          cf = if head cotx2 == 0 then cotx2 else tanx2
>          c  = RealCF cf
> realRealSin x = 2*t / (1+t*t)
>                 where t = tan (x/2)

> realRealCos :: RealReal -> RealReal
> realRealCos rx@(RealCF _)
>  = if head cotx2 == 0 then (c2-1)/(c2+1) else (1-c2)/(1+c2)
>    where (RealCF x2) = rx/2
>          cotx2       = cotR x2
>          tanx2       = tanR x2
>          cf = if head cotx2 == 0 then cotx2 else tanx2
>          c  = RealCF cf
>          c2 = c*c
> realRealCos x = (1-t2) / (1+t2)
>                 where t  = tan (x/2)
>                       t2 = t*t

> realRealTan :: RealReal -> RealReal
> realRealTan (RealInt x) = if x==0 then RealInt 0 else RealCF (tanQ (x%%1))
> realRealTan (RealRat x) = RealCF (tanQ x)
> realRealTan rx@(RealCF _)
>  = RealCF (tanR x)
>    where (RealCF x') = rx / pi
>          (i,_)       = integerFraction x'
>          (RealCF x)  = rx - (RealInt i * pi)

\subsection{Inverse Trig}

> realRealAsin :: RealReal -> RealReal
> realRealAsin x = atan (x / sqrt (1-x*x))

> realRealAcos :: RealReal -> RealReal
> realRealAcos x = atan (sqrt (1-x*x) / x)

> realRealAtan :: RealReal -> RealReal
> realRealAtan (RealInt x) = if x==0 then RealInt 0 else RealCF (atanQ (x%%1))
> realRealAtan (RealRat x) = RealCF (atanQ x)
> realRealAtan (RealCF  x) = RealCF (atanR x)

\subsection{Hyperbolic Trig Functions}

> realRealSinh :: RealReal -> RealReal
> realRealSinh x = (ex - (1 / ex)) / 2
>                  where ex = exp x

> realRealCosh :: RealReal -> RealReal
> realRealCosh x = (ex + (1 / ex)) / 2
>                  where ex = exp x

> realRealTanh :: RealReal -> RealReal
> realRealTanh x = (ex - ex') / (ex + ex')
>                  where ex  = exp x
>                        ex' = 1/ex

\subsection{Inverse Hyperbolic Trig Functions}

> realRealAsinh :: RealReal -> RealReal
> realRealAsinh x = log (x + sqrt (x*x+1))

> realRealAcosh :: RealReal -> RealReal
> realRealAcosh x = log (x + sqrt (x*x-1))

This is not quite true as the root needs to be real.

> realRealAtanh :: RealReal -> RealReal
> realRealAtanh x = log ((1+x)/(1-x)) / 2

\section{@RealReal@ is an instance of @Text@}

> instance Read RealReal where
>   readsPrec p    = readSigned readRealReal
> instance Show RealReal where
>   showsPrec p rx = showString (showRat p (makeRational rx))

> makeRational :: RealReal -> QRational
> makeRational (RealInt x) = (x%%1)
> makeRational (RealRat x) = x
> makeRational (RealCF  x) = cf2Rat x

\subsection{reading @RealReal@'s}

We can't guarantee that the invariant @read(show x) = x@ once we
consider computable real numbers -- so we don't!

> readRealReal :: ReadS RealReal
> readRealReal r = [(RealInt x, s) | (x,s) <- reads r]

\subsection{printing @RealReal@'s}

There's an irritating feature in the Haskell standard prelude that
requires us to deal with the minus sign here rather than leaving it to
the standard mechanism.

> showRat :: Int -> QRational -> String
> showRat p x = if s == "-" && p > 6 then "(" ++ res ++ ")" else res
>               where z     = qRound (x/accuracy)
>                     s     = if z < 0 then "-" else ""
>                     zs    = show (abs z)
>                     l     = length zs
>                     zs'   = pad ++ zs
>                     pad   = if (l <= decimals)
>                             then take (decimals-l+1) zeros else ""
>                     zeros = repeat '0'
>                     (i,f) = splitAt (length zs' - decimals) zs'
>                     res   = s ++ i ++ "." ++ f



