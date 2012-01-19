--                            -*- Mode: Haskell -*- 
-- Copyright 1994 by Peter Thiemann
-- EbnfGrammar.hs --- a simple combinator parser for a grammar in EBNF 
-- Author          : Peter Thiemann
-- Created On      : Tue Aug  3 10:30:03 1993
-- Last Modified By: Peter Thiemann
-- Last Modified On: Mon Dec 27 17:41:17 1993
-- Update Count    : 13
-- Status          : Unknown, Use with caution!
-- 
--------------------------------------------------
-- $Log: EbnfGrammar.hs,v $
-- Revision 1.1  1996/01/08 20:02:34  partain
-- Initial revision
--
-- Revision 1.3  1994/03/15  15:34:53  thiemann
-- added full color support, XColorDB based
--
--Revision 1.2  1993/08/31  12:31:32  thiemann
--reflect changes in type FONT
--
--Revision 1.1  1993/08/17  12:34:29  thiemann
--Initial revision
--
-- $Locker:  $
--------------------------------------------------

module EbnfGrammar (parseAll) where

import Parsers
import Lexer
import AbstractSyntax

parseAll s = [ prod | (prod, []) <- parseFile (lexer (uncomment s)) ]

-- This is the grammar for EBNF
--	File		= {Production}.
--	Production	= Nonterminal [ String ] "=" Term "." .
--	Term		= Factor / "|" .		# alternative
--	Factor		= ExtAtom + .			# sequence
--	ExtAtom		= Atom
--			| Atom "/" Atom			# repetion through Atom
--			| Atom "+".			# at least one repetion
--	Atom		= Nonterminal
--			| String			# terminal string
--			| "(" Term ")"
--			| "[" Term "]"			# an optional Term
--			| "{" Term "}"			# zero or more repetions
--			.
--	String		= "\"" { character } "\"" .
--	Nonterminal	= letter { letter | digit | "_" } .
--	character	= "\\" charesc.

parseFile =		rpt parseProduction		    -- no longer `using` ProdFile

parseProduction =	(satisfy isIdent `thn`
			opt (satisfy isString) `thn`
			expectSymbol "=" `xthn`
			parseTerm `thnx`
			expectSymbol ".") `using`
			\(nt, (ntNames, term)) -> ProdProduction (getIdent nt) (map getString ntNames) term 

parseTerm =		(parseFactor `thn`
			rpt (expectSymbol "|" `xthn` parseFactor))
			`using2` (:) `using` ProdTerm

parseFactor =		(parseExtendedAtom `thn` rpt parseExtendedAtom)
			`using2` (:) `using` ProdFactor

parseExtendedAtom =     parseAtom `thn`
			opt ((expectSymbol "+" `using` \ _ -> ProdPlus)
			     `alt`
			     (expectSymbol "/" `xthn` parseAtom `using` ProdSlash))
				`using2` helper
			where
			helper term [] = term
			helper term [ProdPlus] = ProdRepeat1 term
			helper term [ProdSlash atom] = ProdRepeatWithAtom term atom

parseAtom =		(expectSymbol "(" `xthn`
			parseTerm `thnx`
			expectSymbol ")")
			`alt`
			((expectSymbol "[" `xthn` parseTerm `thnx` expectSymbol "]")
				`using` ProdOption)
			`alt`
			((expectSymbol "{" `xthn` parseTerm `thnx` expectSymbol "}")
				`using` ProdRepeat)
			`alt`
			(satisfy isIdent `using` (ProdNonterminal . getIdent))
			`alt`
			(satisfy isString `using` (ProdTerminal . getString))

expectSymbol c = satisfy test
	where test (Symbol x) = c == x
	      test _ = False
