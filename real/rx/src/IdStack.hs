module IdStack

( IdStack

, emptyIS, globIS

, pushlocs, poplocs, mkglobs

, findidS, changeprecS
, setarityS, setformS

)

where

import Ids

--------------------------------------------------------------------

data IdStack = IS
	[ IdTable ] 	-- stack of local bindings
	IdTable 	-- global environment
	deriving Show

globIS it = IS [] it
emptyIS = globIS emptyIT


--------------------------------------------------------------------

pushlocs (IS locs glob) = IS (emptyIT : locs) glob

poplocs (IS [] glob) = error "cannot pop locals (stack empty)"
poplocs (IS (loc : locs) glob ) = IS  locs glob

mkglobs (IS [] glob) = error "cannot make globals (local stack empty)"
mkglobs (IS (loc : locs) glob ) = IS ( loc : locs) (plusIT glob loc)

--------------------------------------------------------------------

-- lift a function on an IdTable
-- to a function on an IdStack:

-- first look for a local name
-- then default to the global one

wrapIS :: Bool -> (String -> IdTable -> (a, IdTable)) 
	-> (String -> IdStack -> (a, IdStack))

wrapIS def f name (IS [] glob) = 
	let (x, glob') = f name glob
	in  (x, IS [] glob')

wrapIS def f name (IS (loc : locs) glob) =
    if def 

    then -- don't search, rather define new variable right here
	let (x, loc') = f name loc
	in (x, IS (loc' : locs) glob)

    else -- do search
    	case lookupIT loc name of
	    Just _ -> 	let (x, loc') = f name loc
			in (x, IS (loc' : locs) glob)
	    Nothing -> 	let (x, IS locs' glob') 
				= wrapIS def f name (IS locs glob)
			in (x, IS (loc : locs') glob')

--------------------------------------------------------------------


findidS :: Bool -> String -> Kind -> Kind -> IdStack -> (Id, IdStack)
findidS def name look use is = 
	wrapIS def (\ name it -> findid name look use it ) name is

changeprecS :: IdStack -> String -> Int -> Bind -> (Id, IdStack)
changeprecS is name level bind =
	wrapIS False (\ name it -> changeprec it name level bind) name is


setarityS :: IdStack -> String -> Int -> (Id, IdStack)
-- does nothing if arity is already set 
-- (will complain elsewhere if does not agree and switch implicit is off)
setarityS is name ar  =
	wrapIS False (\ name it -> setarity it name ar) name is


setformS :: IdStack -> String -> Form -> (Id, IdStack)
-- does never complain
setformS is name form =
	wrapIS False (\ name it -> setform it name form ) name is

