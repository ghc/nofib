-- this is from ghc/syslib-ghc originally, 













module CharSeq (
	CSeq,
	cNil, cAppend, cIndent, cNL, cStr, cPStr, cCh, cInt,

	cLength,
	cShows,

	cShow


   ) where


cShow	:: CSeq -> [Char]


-- not used in GHC
cShows	:: CSeq -> ShowS
cLength	:: CSeq -> Int


cNil    :: CSeq
cAppend :: CSeq -> CSeq -> CSeq
cIndent :: Int -> CSeq -> CSeq
cNL 	:: CSeq
cStr 	:: [Char] -> CSeq
cPStr	:: String -> CSeq
cCh 	:: Char -> CSeq
cInt	:: Int -> CSeq



data CSeq
  = CNil
  | CAppend	CSeq CSeq
  | CIndent	Int  CSeq
  | CNewline			-- Move to start of next line, unless we're
				-- already at the start of a line.
  | CStr	[Char]
  | CCh		Char
  | CInt	Int	-- equiv to "CStr (show the_int)"


cNil = CNil

-- cAppend CNil cs2  = cs2
-- cAppend cs1  CNil = cs1

cAppend cs1 cs2 = CAppend cs1 cs2

cIndent n cs = CIndent n cs

cNL	= CNewline
cStr	= CStr
cCh	= CCh
cInt	= CInt


cPStr	= CStr


cShow  seq	= flatten (0) True seq []


cShows seq rest = cShow seq ++ rest
cLength seq = length (cShow seq) -- *not* the best way to do this!


data WorkItem = WI Int CSeq -- indentation, and sequence

flatten :: Int	-- Indentation
	-> Bool	-- True => just had a newline
	-> CSeq		-- Current seq to flatten
	-> [WorkItem]	-- Work list with indentation
	-> String

flatten n nlp CNil seqs = flattenS nlp seqs

flatten n nlp (CAppend seq1 seq2) seqs = flatten n nlp seq1 ((WI n seq2) : seqs)
flatten n nlp (CIndent (n2) seq) seqs = flatten (n2 + n) nlp seq seqs

flatten n False CNewline seqs = '\n' : flattenS True seqs
flatten n True  CNewline seqs = flattenS True seqs	-- Already at start of line

flatten n False (CStr s) seqs = s ++ flattenS False seqs
flatten n False (CCh  c) seqs = c :  flattenS False seqs
flatten n False (CInt i) seqs = show i ++ flattenS False seqs


flatten n True  (CStr s) seqs = mkIndent n (s ++ flattenS False seqs)
flatten n True  (CCh  c) seqs = mkIndent n (c :  flattenS False seqs)
flatten n True  (CInt i) seqs = mkIndent n (show i ++ flattenS False seqs)


flattenS :: Bool -> [WorkItem] -> String
flattenS nlp [] = ""
flattenS nlp ((WI col seq):seqs) = flatten col nlp seq seqs

mkIndent :: Int -> String -> String
mkIndent (0) s = s
mkIndent n       s
  = if (n >= (8))
    then '\t' : mkIndent (n - (8)) s
    else ' '  : mkIndent (n - (1)) s
    -- Hmm.. a little Unix-y.



