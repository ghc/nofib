\chapter{Rational Numbers (with Infinity)}

\begin{verbatim}
$Log: QRationals.lhs,v $
Revision 1.2  1996/07/25 21:30:48  partain
Bulk of final changes for 2.01

Revision 1.1  1996/01/08 20:05:18  partain
Initial revision

\end{verbatim}

> module QRationals
>  (QRational, (%%), qNumerator, qDenominator,
>   qInfinite, qUndefined, qRound, qFinite) where
> infixl 7  %% , :%%
> prec = 7::Int

> data  QRational		=  Integer :%% Integer deriving (Eq, Binary)

> qReduce, (%%)			:: Integer -> Integer -> QRational
> qNumerator, qDenominator	:: QRational -> Integer

Dealing with the following two functions' cases for $0$ is the only
change to the standard prelude's rational package.

> qReduce x 0            	=  signum x :%% 0
> qReduce x y			=  (signum x * (abs x `div` d)) :%% (y `div` d)
>			 	   where d = gcd x y

> x %% 0                	=  signum x :%% 0
> x %% y			=  qReduce (x * signum y) (abs y)

> qNumerator (x:%%y)		=  x

> qDenominator (x:%%y)		=  y

> instance Ord QRational where
>     (x:%%y) <= (x':%%y')	=  x * y' <= x' * y
>     (x:%%y) <  (x':%%y')	=  x * y' <  x' * y

> instance Num QRational where
>     (x:%%y) + (x':%%y')	=  qReduce (x*y' + x'*y) (y*y')
>     (x:%%y) - (x':%%y')	=  qReduce (x*y' - x'*y) (y*y')
>     (x:%%y) * (x':%%y')	=  qReduce (x * x') (y * y')
>     negate (x:%% y)		=  (-x) :%% y
>     abs (x:%% y)		=  abs x :%% y
>     signum (x:%% y)		=  signum x :%% 1
>     fromInteger x		=  fromInteger x :%% 1

> instance Real QRational where
>     toRational (x:%%y)	=  x%y

> instance Fractional QRational  where
>     (x:%%y) / (x':%%y')	=  (x*y') %% (y*x')
>     recip (x:%%y)		=  if x < 0 then (-y):%% (-x) else y:%%x
>     fromRational x    	=  numerator x :%% denominator x

> instance  Enum QRational where
>	enumFrom x		= enumFromBy x 1
>	enumFromThen x y	= enumFromBy x (y - x)

> enumFromBy n k		=  n : enumFromBy (n+k) k

> instance  Read QRational  where
>	readsPrec p		=  readParen (p > prec)
>                                   (\r -> [(x%%y,u) | (x,s)    <- reads r,
>                                                      ("%%",t) <- lex s,
>                                                      (y,u)    <- reads t ])
> instance  Show QRational  where
>	showsPrec p (x:%%y)	= showParen (p > prec) (shows x .
>                                                       showString " %% " .
>                                                       shows y)

\section{General extras for Rationals}

> qInfinite   :: QRational -> Bool
> qInfinite x  = qDenominator x == 0 && qNumerator x /= 0

> qUndefined  :: QRational -> Bool
> qUndefined x = qDenominator x == 0 && qNumerator x == 0

> qRound      :: QRational -> Integer
> qRound x     = if qUndefined x || qInfinite x
>                then error "qRound: undefined or infinite"
>                else (2*qNumerator x + d) `div` (2*d) where d = qDenominator x

> qFinite     :: QRational -> Bool
> qFinite x    = qDenominator x /= 0

This concludes the slightly extended rational package.
