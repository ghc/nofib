--                            -*- Mode: Haskell -*- 
-- Copyright 1994 by Peter Thiemann
-- GrammarTransform.hs --- some transformations on parse trees
-- Author          : Peter Thiemann
-- Created On      : Thu Oct 21 16:44:17 1993
-- Last Modified By: Peter Thiemann
-- Last Modified On: Mon Dec 27 17:41:16 1993
-- Update Count    : 14
-- Status          : Unknown, Use with caution!
-- 
-- $Locker:  $
-- $Log: GrammarTransform.hs,v $
-- Revision 1.1  1996/01/08 20:02:35  partain
-- Initial revision
--
-- Revision 1.1  1994/03/15  15:34:53  thiemann
-- Initial revision
--
-- 

module GrammarTransform (simplify) where

import AbstractSyntax

simplify = map simplify' . simp3

-- simp1 gets the body of a ProdFactor as an argument
-- and provides the transformations
--	beta { X } X gamma	--->	beta (X)+ gamma
--	beta X { X } gamma	--->	beta (X)+ gamma
--	beta { X Y } X gamma	--->	beta (X)/ (Y) gamma
--	beta X { Y X } gamma	--->	beta (X)/ (Y) gamma

simp1 [] = []
simp1 [p] = [p]
simp1 (ProdRepeat p:p':prods)
	| p `eqProduction` p' = ProdRepeat1 p: simp1 prods
simp1 (p:ProdRepeat p':prods)
	| p `eqProduction` p' = ProdRepeat1 p: simp1 prods
simp1 (ProdRepeat (ProdFactor [p1, p2]):p:prods)
	| p1 `eqProduction` p = ProdRepeatWithAtom p p2: simp1 prods
simp1 (p:ProdRepeat (ProdFactor [p1, p2]):prods)
	| p `eqProduction` p2 = ProdRepeatWithAtom p p1: simp1 prods
simp1 (p:prods) = p: simp1 prods

-- simp2 gets the body of a ProdTerm as an argument
-- and provides the transformations
--     X gamma | X delta  ---> X (gamma | delta)
--     X gamma | X        ---> X [ gamma ]

simp2 (ProdFactor (p:rest): ProdFactor (p':rest'): more)
	| p `eqProduction` p' = case (rest, rest') of
		([], []) -> simp2 (ProdFactor [p]: more)
		([], _)  -> simp2 (ProdFactor [p, ProdOption (ProdFactor rest')]: more)
		(_,  []) -> simp2 (ProdFactor [p, ProdOption (ProdFactor rest)]: more)
		(_,  _)  -> simp2 (ProdFactor [p, ProdTerm (simp2 [ProdFactor rest, ProdFactor rest'])]: more)
	| otherwise = ProdFactor (p:rest): simp2 (ProdFactor (p':rest'):more)
simp2 [p] = [p]
simp2 [] = []

-- simp3 gets a list of ProdProductions and looks for left and right recursive productions
-- it executes the transformations
--	A -> A gamma_1 | ... | A gamma_k | delta
--	--->
--	A -> delta { gamma_1 | ... | gamma_k }
-- and
--	A -> gamma_1 A | ... | gamma_k A | delta
--	--->
--	A -> { gamma_1 | ... | gamma_k } delta

leftParty nt (ProdTerm ps) = foldr f ([], []) ps
  where f (ProdFactor (ProdNonterminal nt':rest)) (yes, no)
	  | nt == nt' = (ProdFactor rest:yes, no)
        f p (yes, no) = (yes, p:no)

simp3'l prod@(ProdProduction nt nts p@(ProdTerm _))
  = case leftParty nt p of
	(lefties@(_:_), others@(_:_)) ->
		ProdProduction nt nts
		  (ProdFactor [ProdTerm others, ProdRepeat (ProdTerm lefties)])
	_ -> prod
simp3'l prod = prod

rightParty nt (ProdTerm ps) = foldr f ([], []) ps
  where f (ProdFactor ps) (yes, no)
	  | length ps > 1 && rightmost nt ps = (ProdFactor (init ps):yes, no)
	f p (yes, no) = (yes, p:no)

rightmost nt [ProdNonterminal nt'] = nt == nt'
rightmost nt [p] = False
rightmost nt (p:ps) = rightmost nt ps

simp3'r prod@(ProdProduction nt nts p@(ProdTerm _))
  = case rightParty nt p of
	(righties@(_:_), others@(_:_)) ->
		ProdProduction nt nts
		  (ProdFactor [ProdRepeat (ProdTerm righties), ProdTerm others])
	_ -> prod
simp3'r prod = prod

simp3 = map (simp3'r . simp3'l)

-- compute the set of all nonterminals in a Production
freents :: Production -> [String]
freents (ProdTerm prods)           = concat (map freents prods)
freents (ProdFactor prods)         = concat (map freents prods)
freents (ProdNonterminal s)        = [s]
freents (ProdTerminal s)           = []
freents (ProdOption p)             = freents p
freents (ProdRepeat p)             = freents p
freents (ProdRepeat1 p)            = freents p
freents (ProdRepeatWithAtom p1 p2) = freents p1 ++ freents p2
freents (ProdPlus)                 = []
freents (ProdSlash p)              = freents p
--

simplify' (ProdProduction s1 s2 prod)	= ProdProduction s1 s2 (simplify' prod)
simplify' (ProdTerm prods)		= ProdTerm ((simp2 . map simplify') prods)
simplify' (ProdFactor prods)		= ProdFactor (simp1 (map simplify' prods))
simplify' (ProdNonterminal s)		= ProdNonterminal s
simplify' (ProdTerminal s)		= ProdTerminal s
simplify' (ProdOption prod)		= ProdOption (simplify' prod)
simplify' (ProdRepeat prod)		= ProdRepeat (simplify' prod)
simplify' (ProdRepeat1 prod)		= ProdRepeat1 (simplify' prod)
simplify' (ProdRepeatWithAtom prod1 prod2) = ProdRepeatWithAtom (simplify' prod1) (simplify' prod2)
simplify' (ProdPlus)			= ProdPlus
simplify' (ProdSlash prod)		= ProdSlash (simplify' prod)

-- Goferisms:

eqList [] [] = True
eqList (x:xs) (y:ys) = eqProduction x y && eqList xs ys
eqList _ _ = False

eqProduction (ProdFile ps) (ProdFile ps') = eqList ps ps'
eqProduction (ProdProduction str ostr p) (ProdProduction str' ostr' p') = str == str' && ostr == ostr' && eqProduction p p'
eqProduction (ProdTerm ps) (ProdTerm ps') = eqList ps ps'
eqProduction (ProdFactor ps) (ProdFactor ps') = eqList ps ps'
eqProduction (ProdNonterminal str) (ProdNonterminal str') = str == str'
eqProduction (ProdTerminal str) (ProdTerminal str') = str == str'
eqProduction (ProdOption p) (ProdOption p') = eqProduction p p'
eqProduction (ProdRepeat p) (ProdRepeat p') = eqProduction p p'
eqProduction (ProdRepeatWithAtom p1 p2) (ProdRepeatWithAtom p1' p2') = eqProduction p1 p1' && eqProduction p2 p2'
eqProduction (ProdRepeat1 p) (ProdRepeat1 p') = eqProduction p p'
eqProduction (ProdPlus) (ProdPlus) = True
eqProduction (ProdSlash p) (ProdSlash p') = eqProduction p p'
eqProduction _ _ = False
