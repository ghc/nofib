> module Main where

> import Parsers

> infixr 8 +.+ , +.. , ..+
> infixl 7 <<< , <<*
> infixr 6 |||
> 
> (+.+) = thn
> (..+) = xthn
> (+..) = thnx
> (|||) = alt
> (<<<) = using
> (<<*) = using2
> lit   :: Eq a => a -> Parser a a
> lit   = literal
> star  = rpt
> anyC  = satisfy (const True)
> butC cs = satisfy (not.(`elem` cs))
> noC   "" = [("","")]
> noC   _  = []
> unitL x = [x]

----------------------------------------------------------------------

> main :: Dialogue
> main = getArgs exit parse_args

> parse_args :: [String] -> Dialogue
> parse_args (regexp: files) =
> 	let acc = acceptor (fst(head(nnRegexp regexp)))
> 	    acc' = unlines . filter acc . lines
> 	in
> 	    readChan stdin exit (\inp -> 
> 	    appendChan stdout (acc' inp) exit done)
> parse_args _ =
> 	getProgName exit (\progName ->
>	appendChan stderr ("Usage: " ++ progName ++ " regexp\n") exit done)

{-
  Atom		= character | "\\" character | "." | "\\(" Regexp "\\) .
  ExtAtom	= Atom ["*" | "+" | "?"] .
  Factor	= ExtAtom + .
  Regexp	= Factor / "\\|" ["$"].
-}

> data NFANode 
>      	= NFAChar Char [NFANode]
> 	| NFAAny  [NFANode]
>	| NFAEps  [NFANode]
> 	| NFAEnd  [NFANode]
> 	| NFAFinal
>	| NFATable [(Char, [NFANode])] [NFANode] [NFANode] Bool
> 
> nfaChar = NFAChar
> nfaAny  = NFAAny
> nfaEps2 ns1 ns2 = [mkTable [] [] [] False (ns1 ++ ns2)]
> nfaEnd  = NFAEnd
> nfaFinal= NFAFinal
> 
> addPair :: Char -> [NFANode] -> [(Char, [NFANode])] -> [(Char, [NFANode])]
> addPair c ns [] = [(c, ns)]
> addPair c ns ((c',ns'):pairs) | c == c' = (c, ns++ns'):pairs
>                             | otherwise = (c', ns'):addPair c ns pairs

> mkTable pairs anys ends final []      = NFATable pairs anys ends final 
> mkTable pairs anys ends final (NFAChar c n:ns) = mkTable (addPair c n pairs) anys ends final ns
> mkTable pairs anys ends final (NFAAny n:ns) = mkTable pairs (n++anys) ends final ns
> mkTable pairs anys ends final (NFATable pairs' anys' ends' final':ns) = mkTable (pairs'++pairs) (anys'++anys) (ends'++ends) (final' || final) ns
> mkTable pairs anys ends final (NFAEnd n:ns) = mkTable pairs anys (n++ends) final ns
> mkTable pairs anys ends final (NFAFinal:ns) = mkTable pairs anys ends True ns
> mkTable _ _ _ _ _ = error "illegal argument to mkTable"
> 
> type NFAproducer = [NFANode] -> [NFANode]

An NFAproducer takes a list of final states and produces a list of entry states

> nnAtom :: Parser Char NFAproducer
> nnAtom =
>      lit '\\' ..+ lit '(' ..+ nnRegexp +.. lit '\\' +.. lit ')'
>  ||| lit '\\' ..+ butC "|()"	 <<< (\ c -> unitL . nfaChar c)
>  ||| lit '.'			 <<< const (unitL . nfaAny)
>  ||| butC "\\.$"		 <<< (\ c -> unitL . nfaChar c)
>  ||| lit '$' `followedBy` anyC <<< (\ c -> unitL . nfaChar c)

> nnExtAtom :: Parser Char NFAproducer
> nnExtAtom =
>      nnAtom +.+ opt (lit '*' <<< const (\ at finals ->
> 					 let at_inits = at (nfaEps2 finals at_inits)
> 					 in  nfaEps2 at_inits finals)
> 		|||  lit '+' <<< const (\ at finals ->
> 					 let at_inits = at (nfaEps2 finals at_inits)
> 					 in  at_inits)
> 		|||  lit '?' <<< const (\ at finals ->
> 					 let at_inits = at finals
> 					 in  nfaEps2 finals at_inits))
> 	<<< helper
>      where
>        helper (ea, []) = ea
>        helper (ea, [f]) = f ea
> 
> nnFactor :: Parser Char NFAproducer
> nnFactor =
>      plus nnExtAtom	<<< foldr (.) id

> nnRegexp :: Parser Char NFAproducer
> nnRegexp =
>      nnFactor +.+ star (lit '\\' ..+ lit '|' ..+ nnFactor) +.+ opt (lit '$')
> 	<<< helper
>      where
>        helper (ef, (efs, [])) = foldl combine ef efs
>        helper (ef, (efs, _ )) = foldl combine ef efs . unitL . nfaEnd
>	 combine f1 f2 final = nfaEps2 (f1 final) (f2 final)

> nfaStep states c = concat (map step states)
>   where
>     step (NFAChar c' ns') | c == c' = ns'
>     step (NFAAny ns') = ns'
>     step (NFATable pairs anys ends finals) = concat [ n' | (c',n') <- pairs, c == c' ] ++ anys
>     step _ = []

precondition: there are no epsilon cycles!

> epsClosure [] = []
> epsClosure (NFAEps ns:ns') = epsClosure (ns++ns')
> epsClosure (n:ns) = n:epsClosure ns

> acceptor :: NFAproducer -> String -> Bool
> acceptor nfa str = nfaRun (nfa [NFAFinal]) str

> nfaRun ns (c:cs) = nfaRun (nfaStep ns c) cs
> nfaRun ns [] = not (null ((concat (map step ns))))
>   where
>     step (NFAEnd ns') = ns'
>     step (NFAFinal)  = [NFAFinal]
>     step (NFATable pairs anys ends True) = [NFAFinal]
>     step (NFATable pairs anys ends finals) = ends
>     step _           = []

