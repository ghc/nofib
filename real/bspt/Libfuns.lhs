> module Libfuns 

	Module for local dummy parallel annotations

>		(par, plist, seq)
> where


> par x y =  y

> plist l = par (pl l) l
>		where
>		pl [] = []
>		pl (a:l) = par a (pl l)

	
> seq x y = y	
		
