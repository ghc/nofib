module Lexer where
-- Copyright 1994 by Peter Thiemann
import Char -- 1.3

------------------------------------------------------------------------------
--NOW the lexer
------------------------------------------------------------------------------

data Token
	= Ident String | Symbol String | String String

instance Show Token where
  showsPrec n (Ident s)  = showChar '[' . showString s . showString "] "
  showsPrec n (Symbol s) = showChar '<' . showString s . showString "> "
  showsPrec n (String s) = showChar '"' . showString s . showString "\" "
  showList [] = id
  showList (x:xs) = shows x . showList xs

isIdChar c = isAlpha c || isDigit c || c == '_'

theSymbols = "!@#$%^&*+./<=>?\\|:"
isSymbolChar c = c `elem` theSymbols

lexer :: String -> [Token]
lexer "" = []
lexer ('"':cs) = String (stchars): lexer srest
	where (stchars, srest) = lexString cs
lexer ('\'':cs) = String (oneChar): lexer srest
	where (oneChar, srest) = lexChar cs
lexer (c:cs) | isSpace c = lexer cs
	   | isAlpha c = Ident (c:idchars): lexer irest
	   | isSymbolChar c = Symbol(c:sychars): lexer srest
	   | otherwise = Symbol([c]): lexer cs
	where (idchars, irest) = span isIdChar cs
	      (sychars, srest) = span isSymbolChar cs

-- preprocessor for EBNF style comments
uncomment :: String -> String
uncomment ""        = ""
uncomment ('#':cs)  = uncomment (dropWhile (/= '\n') cs)
uncomment ('"':cs)  = '"':uncommentString cs
uncomment ('\'':cs) = '\'':uncommentChar cs
uncomment (c:cs)    = c:uncomment cs

uncommentString "" = ""
uncommentString ('\\':c:cs) = '\\':c:uncommentString cs
uncommentString ('"':cs) = '"':uncomment cs
uncommentString (c:cs) = c:uncommentString cs

uncommentChar "" = ""
uncommentChar ('\\':c:cs) = '\\':c:uncommentChar cs
uncommentChar ('\'':cs) = '"':uncomment cs
uncommentChar (c:cs) = c:uncommentChar cs

-- generic lexers
lexChar ('\\':c:'\'':cs) = ([c], cs)
lexChar (c:'\'':cs) = ([c], cs)
lexChar cs = ([], cs)

lexString ('\\':c:cs) = (c:stchars, srest) where (stchars, srest) = lexString cs
lexString ('"':cs) = ("", cs)
lexString ("") = ("","")
lexString (c:cs) = (c:stchars, srest) where (stchars, srest) = lexString cs

isIdent (Ident _ ) = True
isIdent _ = False

getIdent (Ident s) = s

isString (String _) = True
isString _ = False

getString (String s) = s

