			-*- Mode: Haskell -*-

$Locker:  $
$Log: HappyParser.ly,v $
Revision 1.1  1996/01/08 20:02:36  partain
Initial revision


A happy specification for the happy input language.

> {
> module HappyParser (theHappyParser) where
> import AbstractSyntax
> import Lexer
> }

> %name localHappyParser
> %tokentype { Token' }
> %token
>	id_tok		{ Ident' _ }
>	":"		{ Colon }
>	";"		{ Semicolon }
>	"::"		{ DoubleColon }
>	"%%"		{ DoublePercent }
>	"%"		{ Percent }
>	"|"		{ Bar }
>	"{"		{ OpenBrace }
>	"}"		{ ClosingBrace }
>	any_symbol	{ Symbol' _ }
>	any_string	{ String' _ }

%newline            	{ Symbol "\n" {- no new line token -} }

> %%

> parser :: { [Production] }
> parser
>	: optCode tokInfos "%%" rules optCode
>				{ reverse $4 }

> rules :: { [Production] }
> rules : rules rule	{ $2 : $1 }
>	| rule		{ [$1] }


> rule :: { Production }
> rule 	: id_tok "::" code id_tok ":" prods	{ ProdProduction (getIdent' $4) [] (ProdTerm $6) }
>  	| id_tok ":" prods			{ ProdProduction (getIdent' $1) [] (ProdTerm $3) }


> prods :: { [Production] }
> prods : prod "|" prods			{ $1 : $3 }
>	| prod					{ [$1] }

> prod :: { Production }
> prod	: prodItems code ";"			{ ProdFactor $1 }
>	| prodItems code			{ ProdFactor $1 }

> prodItems :: { [Production] }
> prodItems
>	: prodItem prodItems			{ $1 : $2 }
>	| 					{ [] }

> prodItem :: { Production }
> prodItem
>	: any_string				{ ProdTerminal (getString' $1) }
>	| id_tok				{ ProdNonterminal (getIdent' $1) }

> tokInfos :: { () } 
> tokInfos 
>	: tokInfo tokInfos		{ () }
>	| tokInfo			{ () }

> tokInfo :: { () }
> tokInfo
>	: "%" id_tok tokInfoRest	{ () }

> tokInfoRest :: { () }
> tokInfoRest
>	: code				{ () }
>	| id_tok			{ () }
>	| tokenList			{ () }

> tokenList :: { () }
> tokenList
>	: id_tok code tokenList		{ () }
>	| any_string code tokenList		{ () }
>	| 			{ () }

here goes optCode:

> optCode :: { () }
> optCode 
>	: code			{ () }
>	| 					{ () }

> code :: { () }
> code 	: "{" codeBody "}"	{ () }

> codeBody :: { () }
> codeBody
>	: codeItem codeBody	{ () }
>       | codeItem		{ () }

> codeItem :: { () }
> codeItem
>	: any_string		{ () }
>	| id_tok		{ () }
>	| code			{ () }
>	| any_symbol		{ () }
>	| ":"    { () }
>	| ";"	 { () }
>	| "::"   { () }
>	| "|"    { () }
>	| "%%"	 { () }
>	| "%"	 { () }

> {

> happyError :: Int -> Int -> [Token'] -> a
> happyError s i ts = error ("Parse error in line " ++ show i ++
>                          " [state " ++ show s ++ "]" ++
>                          case ts of
>                          [] -> " (at EOF)\n"
>                          _  ->  "\n" ++ show (take 20 ts) ++ "\n")

A preprocessor for literal scripts (slow)

> unlit :: String -> String
> unlit = unlines . map (tail.tail) . filter p . lines
>     where p ('>':' ':_)  = True
>           p ('>':'\t':_) = True
>           p _            = False

A postprocessor to make happy happy.

> data Token' = Ident' String | Symbol' String | String' String
>             | Percent | DoublePercent | OpenBrace | ClosingBrace
>             | Colon | Semicolon | DoubleColon | Bar

> instance Text Token' where
>   showsPrec n (Ident' s) = showChar '[' . showString s . showString "] "
>   showsPrec n (Symbol' s) = showChar '<' . showString s . showString "> "
>   showsPrec n (String' s) = showChar '"' . showString s . showString "\" "
>   showsPrec n Percent = showString "% "
>   showsPrec n DoublePercent = showString "%% "
>   showsPrec n OpenBrace  = showString "{ "
>   showsPrec n ClosingBrace = showString "} "
>   showsPrec n Colon = showString ": "
>   showsPrec n Semicolon = showString "; "
>   showsPrec n DoubleColon = showString ":: "


> postlexer = map f
>   where f (Symbol "%%") = DoublePercent
>         f (Symbol "%")  = Percent
>         f (Symbol "{")  = OpenBrace
>         f (Symbol "}")  = ClosingBrace
>         f (Symbol "::") = DoubleColon
>         f (Symbol ":")  = Colon
>         f (Symbol ";")  = Semicolon
>         f (Symbol "|")  = Bar
>         f (Symbol s)    = Symbol' s
>         f (Ident  s)    = Ident' s
>         f (String s)    = String' s

> getIdent' (Ident' x) = x
> getString' (String' x) = x

> theHappyParser = localHappyParser . postlexer . lexer . unlit

> }
