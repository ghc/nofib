module Ids 

( Id(..)
, Form(..)

, Kind(..)
, Bind(..)

, changeprec
, setarity
, setform

, idname
, idform

, idarity	-- returns Int
, maybe_idarity	-- returns Maybe Int

, idlook 
, iduse  
, idprec 
, idbind 

, findid

, mkid 
, mknat

, userfun
, uservar
, usercon


, ppfn, ppop

, IdTable

, inIts
, emptyIT
, lookupIT
, addToIT
, plusIT

)

where

-- import Trace

import Maybes

import Char (isDigit)

import FiniteMap
import Pretty
import PrettyClass

import Lex (updown)

import Options

data Kind = Fn | Op 
    deriving (Eq, Show)

data Bind = Lft | Rght | Nn	-- associativity
    deriving (Eq, Show)

data Form 
	= Passive String 	-- is just expanded
	| Active Int String	-- arguments get plugged in

data Id = Id 
	String	-- name (that is typed in)
	Form	-- latex expansion (may contain #1, #2 ...)
		-- for expansions of arguments
	(Maybe Int) -- arity 
	Kind	-- looks like Fn or Op
	Kind	-- used as Fn or Op
	(Maybe Int) -- precedence
	Bind	-- Associativity

-- won't need this much, except for debugging
instance Show Id where showsPrec p id = showString (idname id)


instance Eq Id where
  x == y = idname x == idname y	-- disregarding all other fields

instance Ord Id where
  x <= y = idname x <= idname y	-- disregarding all other fields

----------------------------------------------


ppfn opts id = caseopts opts "code"
    [ ("plain", case idlook id of
	Fn -> ppStr (       idname id       )
	Op -> ppStr ("(" ++ idname id ++ ")") )

    , ("latex",
	let f = case idform id of 
			Active _ _ -> idname id -- shouldn't happen
			Passive form -> form
	in case idlook id of
		Fn -> ppStr (       f       )
		Op -> ppStr ("(" ++ f ++ ")") )
    ]

ppop opts id = caseopts opts "code"
    [ ("plain", case idlook id of
	Fn -> ppStr ("`" ++ idname id ++ "`")
	Op -> if idname id == getopt opts "apply" && onoff opts "implicit"
	      then ppNil 
	      else ppStr (idname id)
      )
    , ("latex",
	let f = case idform id of 
			Active _ _ -> idname id -- shouldn't happen
			Passive form -> form
	in case idlook id of


--		Fn -> ppStr ("`" ++ f ++ "`")
-- TODO: something more clever here
		Fn -> ppStr (       f       )


		Op -> ppStr (       f       ) )
    ]

-----------------------------------------------


-- todo: use records here ?
idname  (Id name form arity look use prec bind) = name
idform  (Id name form arity look use prec bind) = form
maybe_idarity (Id name form arity look use prec bind) = arity
idlook  (Id name form arity look use prec bind) = look
iduse   (Id name form arity look use prec bind) = use
idprec  (Id name form arity look use prec bind) = prec
idbind  (Id name form arity look use prec bind) = bind

idarity id = case maybe_idarity id of
	Just n -> n
	Nothing -> error ("no arity for " ++ show id)

mkid :: String -> Form -> Maybe Int -> Kind -> Kind -> Maybe Int -> Bind 
	-> Id
mkid name form arity look use prec bind = 
	Id name form arity look use prec bind 

mknat :: Int -> Id
mknat n = mkid (show n) (Passive (show n)) 
	(Just 0) Fn Fn Nothing Nn   

userfun :: Int -> String -> Id
userfun n name = mkid name (Passive (pform "\\mathrm" name))
	(Just n) Fn Fn Nothing Nn

uservar ::        String -> Id
uservar   name  = mkid name (Passive (pform "\\mathit" name))
	(Just 0) Fn Fn Nothing Nn
	
usercon :: Int -> String -> Id
usercon n name =  mkid name (Passive (pform "\\mathbf" name))
	(Just n) Fn Fn Nothing Nn

--------------------------------------------------

data IdTable = IT (FiniteMap String Id) deriving Show

emptyIT = IT (emptyFM)

lookupIT (IT fm) = lookupFM fm

addToIT (IT fm) name id =
	IT (addToFM_C (error "addToIt") fm name id)

plusIT (IT fm1) (IT fm2) = 
	IT (plusFM_C (error "Ids.plusIT: IdTables not disjoint") fm1 fm2)

inIts :: [Id] -> IdTable
inIts ids = IT (listToFM [ (idname id, id) | id <- ids ])

-------------------------------------------------



pform style name | isDigit (head name) = name

pform style name | length name == 1 =
	if style == "\\mathit" 
	then name -- one letter ids are mathit automatically
	else style ++ "{" ++ name ++ "}"

pform style name = 
    let (as, bs) = span (not . updown) name
	(cs, ds) = span (      updown) bs
    in	style ++ "{" ++ as ++ "}"
	++ case bs of
		[] -> ""
		_ -> case ds of
		    	[] -> cs	-- hopefully these are primes ''''
					-- otherwise, just "_" or "^" (we hope)
			_ -> cs ++ "{" ++ pform style ds ++ "}"

-----------------------------------------------------------

findid :: String -> Kind -> Kind -> IdTable -> (Id, IdTable)
findid name look use it =
    case lookupIT it name of
	Just id -> (id, it)
	Nothing -> 
	    let	id = mkid name 
			(case look of -- tex form
				Fn -> Passive (pform "\\mathit" name)
				Op -> Passive ("\\verb;"   ++ name ++ ";")
			)
			(case use of -- arity
				Fn -> Nothing
				Op -> Just 2	
			)
			look
			use
			Nothing	-- precedence
			Nn	-- associativity
	    in
--		trace ("\nmaking " ++ show id) $
		(id, addToIT it name id)

changeprec :: IdTable -> String -> Int -> Bind -> (Id, IdTable)
changeprec it @ (IT fm) name level bind =
    let id' = lookupFM fm name
    in	if exists id' && not (exists (idprec (the id')))
	then 
	    let id'' = mkid
		 	(idname (the id')) (idform (the id')) 
			(maybe_idarity (the id'))
			(idlook (the id')) (iduse (the id'))
		 	(Just level) 
			bind 
	    in	(id'', IT (addToFM fm name id''))

	else error ("cannot change precedence of: " ++ name)

setarity :: IdTable -> String -> Int -> (Id, IdTable)
-- does nothing if arity is already set 
-- (will complain elsewhere if does not agree and implicit_apply is off)
setarity it @ (IT fm) name ar  =
    let id' = lookupFM fm name
    in	if exists id' 
	then
	    if exists (maybe_idarity (the id'))
	    then (the id', it) 	-- needs no change
	    else
		    let id'' = mkid
	 			(idname (the id')) (idform (the id')) 
				(Just ar)
				(idlook (the id')) (iduse (the id'))
		 		(idprec (the id')) (idbind (the id')) 
		    in 	(id'', IT (addToFM fm (name) id''))

	else error ("setarity: id doesn't exist: " ++ name)


setform :: IdTable -> String -> Form -> (Id, IdTable)
-- does never complain
setform it @ (IT fm) name cs =
    let id' = lookupFM fm name
    in	if exists id' 
	then
		    let id'' = mkid
	 			(idname (the id')) cs 
				(maybe_idarity (the id'))
				(idlook (the id')) (iduse (the id'))
		 		(idprec (the id')) (idbind (the id')) 
		    in 	(id'', IT (addToFM fm (name) id''))

	else error ("setform: id doesn't exist: " ++ name)




