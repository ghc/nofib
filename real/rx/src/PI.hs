module PI

( PI
, PIS

, PY	-- export required by hbc ???

, llit, llitp
, lmany, lmany1
, lsepBy, lsepBy1

, lparse, opt

, getopts

, makeid, makenat
, putprec, putarity, putform

, pushlocals, poplocals, mkglobals

, makeidS, makenatS
, putprecS, putarityS, putformS

)

where

-- import Trace

import Options
import Ids

import IdStack

import Parse	-- from syslib hbc



-- data PI v = PI ((Opts, IdTable) -> Parser [String] (v, (Opts, IdTable)))
-- unPI (PI p) = p

data PY a v = PY (a -> Parser [String] (v, a))
unPY (PY p) = p

type PI v = PY (Opts, IdTable) v
type PIS v = PY (Opts, IdStack) v

------------------------------------------------------------------------

instance Functor (PY a) where
    map f (PY p) = PY (\ x -> 
	p x `act` (\ (v, x) -> (f v, x)))

instance Monad (PY a) where
    return r = PY ( \ x -> succeed (r, x) )
    PY p >>= g = PY (\ x -> 
	p x `into` (\ (v, x') -> unPY (g v) x'))

instance MonadZero (PY a) where
    zero = PY (\ x -> failP "PY.zero")

instance MonadPlus (PY a) where
    (PY p) ++ (PY q) = PY ( \ x -> p x ||! q x )

--------------------------------------------------------------------------

lparse (PY p) x toks = parse (p x) toks

    
--------------------------------------------------------------------------

getopts :: PY (a, b) a
getopts = PY (\ (o, i) -> succeed (o, (o, i)))

--------------------------------------------------------------------------

makenat :: Int -> PI Id
makenat n = makeid (show n) Fn Fn



makeid :: String -> Kind -> Kind -> PI Id
makeid name look use = PY ( \ (o, i) -> 
	let (id, i') = findid name look use i
	in succeed (id, (o, i')) )

putprec id level bind = PY ( \ (o, i) ->
	let (id', i') = changeprec i id level bind
	in succeed (id', (o, i')) )

putarity id ar = PY ( \ (o, i) ->
	let (id', i') = setarity i id ar
	in 
--		trace ("\nputarity.id : " ++ show id) $
--		trace ("\nputarity.id.arity : " ++ show (maybe_idarity id)) $
--		trace ("\nputarity.ar : " ++ show ar) $
--		trace ("\nputarity.id'.arity : " ++ show (idarity id')) $
		succeed (id', (o, i')) )

putform id cs = PY ( \ (o, i) ->
	let (id', i') = setform i id cs
	in succeed (id', (o, i')) )

-----------------------------------------------------------------------

makenatS :: Bool -> Int -> PIS Id
makenatS def n = makeidS def (show n) Fn Fn

makeidS :: Bool -> String -> Kind -> Kind -> PIS Id
makeidS def name look use = PY ( \ (o, i) -> 
	let (id, i') = findidS def name look use i
	in succeed (id, (o, i')) )

putprecS id level bind = PY ( \ (o, i) ->
	let (id', i') = changeprecS i id level bind
	in succeed (id', (o, i')) )

putarityS id ar = PY ( \ (o, i) ->
	let (id', i') = setarityS i id ar
	in 
--		trace ("\nputarity.id : " ++ show id) $
--		trace ("\nputarity.id.arity : " ++ show (maybe_idarity id)) $
--		trace ("\nputarity.ar : " ++ show ar) $
--		trace ("\nputarity.id'.arity : " ++ show (idarity id')) $
		succeed (id', (o, i')) )

putformS id cs = PY ( \ (o, i) ->
	let (id', i') = setformS i id cs
	in succeed (id', (o, i')) )

---------------------------------------------------------------------


lift :: Parser [String] v -> PY a v
lift p = PY (\ x -> p `act` \ v -> (v, x) )

---------------------------------------------------------------------

pushlocals :: PIS ()
pushlocals = PY ( \ (o, i) -> succeed ((), (o, pushlocs i)) )

poplocals :: PIS ()
poplocals = PY ( \ (o, i) -> succeed ((), (o, poplocs i)) )

mkglobals :: PIS ()
mkglobals = PY ( \ (o, i) -> succeed ((), (o, mkglobs i)) )


---------------------------------------------------------------------

llit x = lift (lit x)

llitp msg p = lift (litp msg p)

lmany1 p = do { x <- p; xs <- lmany p; return (x : xs) }
lmany  p = lmany1 p ++ return []

p `lsepBy1` q = 
    do 	{ x <- p
	; ys <- lmany ( do { q; y <- p; return y } )
	; return (x : ys)
	}

p `lsepBy` q = p `lsepBy1` q ++ return []


opt p = map Just p ++ return Nothing

----------------------------------------------------------------------


