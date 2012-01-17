

module Lex

( uncomment
, isDel
, isAlphanum', updown

, myLex

)

where

import Data.Char


--------------------------------------------------------

uncomment :: String -> String
uncomment [] = []
uncomment ('-' : '-' : cs) = uncomment (dropWhile (/= '\n') cs)
uncomment ('{' : '-' : cs) = recomment cs
uncomment (c : cs) = c : uncomment cs

recomment :: String -> String
recomment [] = []
recomment ('-' : '-' : cs) = recomment (dropWhile (/= '\n') cs)
recomment ('-' : '}' : cs) = uncomment cs
recomment (c : cs) = recomment cs

-------------------------------------------------------

-- treat TeX operators 
updown c = c `elem` "_^'"

isAlphanum' c = isAlphaNum c || updown c

-------------------------------------------------------


myLex [] = []

myLex ('"' : cs) = 
    let (as, bs) = span (/= '"') cs
    in  ('"' : as ++ "\"") : myLex (drop 1 bs)


myLex (c : cs) | isSpace c = myLex cs
myLex (c : cs) | isAlpha c =
    	let (ds, es) = span isAlphanum' cs
        in (c : ds) : myLex es
myLex (c : cs) | isDigit c =
    	let (ds, es) = span isDigit cs
        in (c : ds) : myLex es
myLex (c : cs) | isDel c =
	[c] : myLex cs

myLex (c : cs) = 
	let (ds, es) = break (\ c -> isAlphanum' c || isSpace c || isDel c) cs
	in (c : ds) : myLex es

----------------------------------------------------------------------------

isDel '(' = True; isDel ')' = True
isDel '[' = True; isDel ']' = True
isDel '{' = True; isDel '}' = True
isDel '`' = True
isDel '"' = True
isDel ',' = True

-- isDel ';' = True  NOT: semicolon is an operator, has semantics

isDel _ = False


