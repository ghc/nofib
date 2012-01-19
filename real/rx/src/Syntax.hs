-- functions and operators


module Syntax

( CType(..)
, Exp(..)



, appId, appArgs, appids
, isApp, isAppId, unAppId


, pr

, substExp

, cType, cArgs, isColl

)

where

-- import List
import Maybes

import Ids

import Pretty 		-- syslib ghc
import PrettyClass 

import Options		-- to find out about print format

import FiniteMap

-----------------------------------------------------------------------

data CType = CSet | CList | CTuple
	deriving (Eq, Ord, Show)

data Exp 
	= App Id [Exp]  -- function (identifier) application
	| Coll CType [Exp]

	deriving (Eq, Ord)

isApp (App _ _) = True; isApp _ = False
isColl (Coll _ _) = True; isColl _ = False

cType (Coll ct _) = ct
cArgs (Coll _ ca) = ca

appId (App id args) = id
appArgs (App id args) = args

isAppId (App id []) = True; isAppId _ = False
unAppId (App id []) = id; unAppId _ = error "unAppId"

appids (App id xs) = id : concat (map appids xs)
appids (Coll _ xs) =      concat (map appids xs)

------------------------------------------------------------------



substExp :: Exp -> Exp -> Exp -> Exp

substExp a val x | a == x = val
substExp a val (App id xs) = App id ( map (substExp a val) xs )
substExp a val (Coll t xs) = Coll t ( map (substExp a val) xs )


----------------------------------------------

paren opts f p = if f then alParens opts p else p
brack opts f p = if f then alBrackets opts p else p
curls opts f p = if f then alBraces opts p else p

lgroup :: Pretty -> Pretty
lgroup p = ppBesides [ppStr "{", p, ppStr "}"]

instance Show Exp where showsPrec p = emitascii

-- todo: something more distinctive
pr opts = pp opts

instance PrettyClass Exp where

    ppp opts p (Coll tc args) = (case tc of
	CSet -> curls ; CList -> brack; CTuple -> paren) 
	opts True (ppCommas (map (pp opts) args))
	

    ppp opts p (App f args) = 
     	if null args
	then ppfn opts f

	else case idform f of
	  Active _ _ -> caseopts opts "code"
			[ ("latex", activate opts p f args)
			, ("plain", passivate opts p f args)
			]
	  Passive _ -> passivate opts p f args

activate :: Opts -> Int -> Id -> [ Exp ] -> Pretty
activate opts p f args =
    let	Active n cs = idform f

	fs :: FiniteMap Int Pretty
	fs = 	if length args /= n 
		then error ("active form used with wrong number of args, "
				++ show f ++ show args)
		else listToFM (zip [1..n] 
			[lgroup (ppp opts 0 arg) | arg <- args])
		-- note: individual args are formatted with
		-- surrounding precedence level 0

	atoi :: Char -> Int
	atoi c = fromEnum c - fromEnum '0'

	farg :: Int -> Pretty
	farg i = lookupWithDefaultFM fs 
		(error ("arg no " ++ show i ++ " missing")) i
		
	eat :: String -> Pretty
	eat "" = ppNil
	eat ('#' : c : cs) = farg (atoi c) `ppBeside` eat cs
	eat (c : cs) = ppChar c `ppBeside` eat cs

    in	eat cs



passivate :: Opts -> Int -> Id -> [ Exp ] -> Pretty
passivate opts p f args =
     if  iduse f == Fn 
     then paren opts (p == 100)
		(ppfn opts f `ppSep2`
			ppNest tabstop (ppSepp 
				[ ppp opts 100 arg | arg <- args ])
			)
     else case args of
		[x, y] -> props opts p f x y
		_ -> error "in ppp: op needs exactly 2 args"


props opts p f x y =
    case idprec f of
	Nothing -> paren opts (0 < p) 	-- todo: 100 more abstract
	    		(ppp opts 100 x `ppSep2` ppNest tabstop 
				(ppop opts f `ppSep2` (ppp opts 100 y)))
	Just q ->
	    let qx = q + offset Lft f x
		qy = q + offset Rght f y
    	    in	paren opts (q < p)
		    	(ppp opts qx x `ppSep2` ppNest tabstop 
				(ppop opts f `ppSep2` (ppp opts qy y) ))


offset dir f (App id args) = 
    if idlook id == Fn then 0	-- harmless
    else if idprec id == Nothing then 0	-- will get parens anyway
    else if the (idprec id) /= the (idprec f) then 0 -- precs are distinct
    else if id /= f then 1	-- same precs, different ops: need parens
    else if idbind f == dir then 0	-- i am assoc, need no parens
    else 1	-- i am not assoc, need paren


{-
    ppp LaTeX p (App f args) = 
	let ff = idform f
	    fargs = [ lgroup (pp LaTeX arg) | arg <- args ]

	    expand "" = ppStr ""
	    expand ('#' : c : cs) = 
		let n = fromEnum c - fromEnum '0'
		in (fargs !! (n - 1)) `ppBeside` (expand cs)
	    expand (c : cs) = ppChar c `ppBeside` expand cs

	in  expand ff

-}

{-
    ppp st _ (Let x b) =
	ppSep 	[ ppStr "let", ppNest 4 (pp st b)
		, ppStr "in", ppNest 4 (pp st x) ]
-}

{-
    ppp Ascii p (Con x y) = paren Ascii (conprec < p) 
--  for debugging, show constructors:
--	(ppSep [ ppp Ascii conprec x, ppStr "^", ppp Ascii (conprec + 1) y ])
	(ppSep [ ppp Ascii conprec x,            ppp Ascii (conprec + 1) y ])

    ppp LaTeX p (Con x y) = paren LaTeX (conprec < p) 
	(ppBesides [ ppStr "\\con"
-- make precedences in constructor args very low
-- in order to avoid parentheses that are visually unnecessary
		, lgroup (ppp LaTeX 0 x)
		, lgroup (ppp LaTeX 0 y) 
		])
-}

{-
    ppp st p (Bpp op (arg : args)) =
	let q = opprec op 
	in paren st (q < p) 

-- todo: check whether to hide application
-- todo: do precedences correctly

	    ( ppp st q arg `ppSep2`
		ppNest tabstop
		  (ppSepp [ ppp st q op `ppSep2` ppp st (q+1) arg 
			  | arg <- args ] ))
-}
