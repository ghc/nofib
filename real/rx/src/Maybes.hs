--%
--% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996

-- with changes by myself (joe@informatik.uni-jena.de)

--%
--\section[Maybes]{The `Maybe' types and associated utility functions}
--


module Maybes (

	exists, the,	-- this is missing in 1.4 ?


--	Maybe(..), -- no, it's in 1.3
	MaybeErr(..),

	allMaybes,
	firstJust,
	expectJust,
	maybeToBool,

	assocMaybe,
	mkLookupFun, mkLookupFunDef,

	failMaB,
	failMaybe,
	seqMaybe,
	returnMaB,
	returnMaybe,
	thenMaB



	, findJust
	, foldlMaybeErrs
	, listMaybeErrs

    ) where


-- import Maybe -- renamer will tell us if there are any conflicts


exists = maybeToBool

the (Just x) = x; the Nothing = error "the"

--
--
--%************************************************************************
--%*									*
--\subsection[Maybe type]{The @Maybe@ type}
--%*									*
--%************************************************************************
--
maybeToBool :: Maybe a -> Bool
maybeToBool Nothing  = False
maybeToBool (Just x) = True
--
--@catMaybes@ takes a list of @Maybe@s and returns a list of
--the contents of all the @Just@s in it.	@allMaybes@ collects
--a list of @Justs@ into a single @Just@, returning @Nothing@ if there
--are any @Nothings@.
--


allMaybes :: [Maybe a] -> Maybe [a]
allMaybes [] = Just []
allMaybes (Nothing : ms) = Nothing
allMaybes (Just x  : ms) = case (allMaybes ms) of
			     Nothing -> Nothing
			     Just xs -> Just (x:xs)
--
--@firstJust@ takes a list of @Maybes@ and returns the
--first @Just@ if there is one, or @Nothing@ otherwise.
--
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x  : ms) = Just x
firstJust (Nothing : ms) = firstJust ms
--
findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f []	  = Nothing
findJust f (a:as) = case f a of
		      Nothing -> findJust f as
		      b	 -> b
--
expectJust :: String -> Maybe a -> a
{- not # INLINE expectJust #-}
expectJust err (Just x) = x
expectJust err Nothing  = error ("expectJust " ++ err)
--
--The Maybe monad
--~~~~~~~~~~~~~~~
seqMaybe :: Maybe a -> Maybe a -> Maybe a
seqMaybe (Just x) _  = Just x
seqMaybe Nothing  my = my

returnMaybe :: a -> Maybe a
returnMaybe = Just

failMaybe :: Maybe a
failMaybe = Nothing
--
--Lookup functions
--~~~~~~~~~~~~~~~~
--
--@assocMaybe@ looks up in an assocation list, returning
--@Nothing@ if it fails.
--
assocMaybe :: (Eq a) => [(a,b)] -> a -> Maybe b

assocMaybe alist key
  = lookup alist
  where
    lookup []		  = Nothing
    lookup ((tv,ty):rest) = if key == tv then Just ty else lookup rest


--
--@mkLookupFun eq alist@ is a function which looks up
--its argument in the association list @alist@, returning a Maybe type.
--@mkLookupFunDef@ is similar except that it is given a value to return
--on failure.
--
mkLookupFun :: (key -> key -> Bool)	-- Equality predicate
	    -> [(key,val)] 		-- The assoc list
	    -> key 			-- The key
	    -> Maybe val		-- The corresponding value

mkLookupFun eq alist s
  = case [a | (s',a) <- alist, s' `eq` s] of
      []    -> Nothing
      (a:_) -> Just a

mkLookupFunDef :: (key -> key -> Bool)	-- Equality predicate
	       -> [(key,val)] 		-- The assoc list
	       -> val 			-- Value to return on failure
	       -> key 			-- The key
	       -> val			-- The corresponding value

mkLookupFunDef eq alist deflt s
  = case [a | (s',a) <- alist, s' `eq` s] of
      []    -> deflt
      (a:_) -> a
--
--%************************************************************************
--%*									*
--\subsection[MaybeErr type]{The @MaybeErr@ type}
--%*									*
--%************************************************************************
--
data MaybeErr val err = Succeeded val | Failed err
--
thenMaB :: MaybeErr val1 err -> (val1 -> MaybeErr val2 err) -> MaybeErr val2 err
thenMaB m k
  = case m of
      Succeeded v -> k v
      Failed e	  -> Failed e

returnMaB :: val -> MaybeErr val err
returnMaB v = Succeeded v

failMaB :: err -> MaybeErr val err
failMaB e = Failed e
--
--
--@listMaybeErrs@ takes a list of @MaybeErrs@ and, if they all succeed, returns
--a @Succeeded@ of a list of their values.  If any fail, it returns a
--@Failed@ of the list of all the errors in the list.
--
listMaybeErrs :: [MaybeErr val err] -> MaybeErr [val] [err]
listMaybeErrs
  = foldr combine (Succeeded [])
  where
    combine (Succeeded v) (Succeeded vs) = Succeeded (v:vs)
    combine (Failed err)  (Succeeded _)	 = Failed [err]
    combine (Succeeded v) (Failed errs)	 = Failed errs
    combine (Failed err)  (Failed errs)	 = Failed (err:errs)
--
--@foldlMaybeErrs@ works along a list, carrying an accumulator; it
--applies the given function to the accumulator and the next list item,
--accumulating any errors that occur.
--
foldlMaybeErrs :: (acc -> input -> MaybeErr acc err)
	       -> acc
	       -> [input]
	       -> MaybeErr acc [err]

foldlMaybeErrs k accum ins = do_it [] accum ins
  where
    do_it []   acc []	  = Succeeded acc
    do_it errs acc []	  = Failed errs
    do_it errs acc (v:vs) = case (k acc v) of
			      Succeeded acc' -> do_it errs	 acc' vs
			      Failed err     -> do_it (err:errs) acc  vs
