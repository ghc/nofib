module Grammar 

( Grammar 

)

where

import TA

type Grammar a = (a, [(a, Either a (STerm a))])	-- perhaps some eps moves



