> module GeomNum 

	Defines the Geometric Number class and gives instances
	for LazyRationals only at present. 

>		(GeomNumb(..),Numb(..),Rationals,grid,rnd2
>	)

> where

> import Params (renderLeft,renderTop,renderHeight,windowWidth)
> import Rationals (Rationals(..),rndNR)


        Numb is a type synonym used throughout the rest of
        the Geometric modeller code. A new number system can
        hence be easily replaced by changing this one line.

> type Numb = Rationals

> class (Fractional a) => GeomNumb a where

	GeomNumb Class specifies:
		rnd - How to round a co-ordinate into an Int
		[y,x]CoordInRange - how to work out if a co_ordinate is on screen
				(screen defined in Params.hs)
		ratio - find the ratio of two number in simplest terms
		zero, positive, negative - methods for determining the sign of a number
		fromInt - How to coerce an integer the number type.

> 	rnd :: a->Int
> 	zero,positive,negative :: a->Bool
>	ratio :: a -> a -> (a,a)
> 	xCoordInRange :: a->Bool
> 	yCoordInRange :: a->Bool


> instance GeomNumb Rationals where

>	rnd = rndNR
>	zero a = (a==0)
>	positive a = signum a > 0
>	negative a = signum a < 0
>    	ratio 0 y = if (positive y) then (0,1) else (0,-1)
>       ratio x 0 = if (positive x) then (1,0) else (-1,0)
>       ratio x y = (signum x*(n:%%1),signum y*(d:%%1))
>                       where (n:%%d)= abs x/abs y 
>	xCoordInRange x = (fromInt renderLeft) <= x && x <= (fromInt windowWidth)
>	yCoordInRange x = (fromInt renderTop) <= x && x <= (fromInt renderHeight)

> rnd2 :: Numb -> Numb
> rnd2 a = rnd a :%% 1

> grid :: Numb -> Numb
> grid (x:%%1) = (div (x+5) 10 * 10) :%% 1
