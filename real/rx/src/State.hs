module State

( Sym, dosym
, gensym, push
)

where


-- state Monad ----------------------------------------------------

data Sym s a = Sym (s -> (s, a))

dosym :: Sym (Int, [s]) a -> (a, [s])
-- start computation, show effect
dosym (Sym f) = let ((_, x), r) = f (0, []) in (r, x)

instance Functor (Sym s) where 
	map f (Sym s) = Sym (\ c -> 
		let (d, a) = s c in (d, f a) )

instance Monad (Sym s) where
    return x = Sym (\ c -> (c, x))
    Sym x >>= f = Sym (\ c -> 

-- phorward state is this:
	let (d, r) = x c; Sym y = f r; (e, s) = y d in (e, s) )

-- but we're using backward state (NOT)
--	let (d, s) = y c; Sym y = f r; (e, r) = x d in (e, s) )

-- used for symbol supply
gensym :: Sym (Int, a) String
gensym = Sym (\ (c,x) -> ((c+1,x), "$" ++ show c))

-- remember a result
push :: a -> Sym (b, [a]) ()
push x = Sym ( \ (c, xs) -> ((c, x : xs), () ))

