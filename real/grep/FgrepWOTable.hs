module Main where

main :: Dialogue
main = getArgs exit parse_args

parse_args :: [String] -> Dialogue
parse_args (subject: files) =
	let acc = mkAcceptor subject
	    acc' = unlines . filter acc . lines
	in
	    readChan stdin exit (\inp -> 
	    appendChan stdout (acc' inp) exit done)
parse_args _ = done

type Acceptor  = String -> Bool
type NAcceptor = Char -> Acceptor

mkAcceptor :: String -> Acceptor
mkAcceptor subject = let acc = mkAccept subject [("",\c -> acc)] [""] in acc

mkAccept :: String -> [(String, NAcceptor)] -> [String] -> Acceptor
-- mkAccept subject prefixes suffixes
-- construct an acceptor for subject where
--  prefixes associates consumed prefixes (in reverse) to acceptors ordered by decreasing length
--  suffixes lists the suffixes (in reverse) of the consumed prefix
mkAccept ""     pfx sfx = final
mkAccept (c:cs) pfx sfx = accy
	where
	  lpf  = fst (head pfx)
	  npfx = (c:lpf, accn): pfx
	  nsfx = map (c:) sfx ++ [""]
	  y    = mkAccept cs npfx nsfx
	  n    = findFirst pfx (tail nsfx)
	  accy = qyfun c y n
	  accn = qnfun c y n

-- `qyfun c0 y n' is a success continuation
-- qyfun accepts
--	a character c0 to match
--	a success continuation y (an acceptor for the rest of the string)
--	a failure continuation n (takes the character that failed to match and
--		produces an acceptor, finally)
--	a string, the object to match
-- and produces a Bool

qyfun :: Char -> Acceptor -> NAcceptor -> Acceptor
qyfun c0 y n (c:cs) | c0 == c = y cs
                  | otherwise = n c cs
qyfun c0 y n [] = False

-- `qnfun c0 y n' is a failure continuation
-- qnfun accepts
--	a character c0 to match
--	a success continuation y (an acceptor for the rest of the string)
--	a failure continuation n (takes the character that failed to match and
--		produces an acceptor, finally)
--	the character c to be matched against c0
--	a string, which is matched after c has been matched against c0
-- and produces a Bool

qnfun :: Char -> Acceptor -> NAcceptor -> NAcceptor
qnfun c0 y n c cs = if c == c0 then y cs else n c cs

final :: Acceptor
final _ = True

-- findFirst implements the error function:
-- it returns the state after the longest consumed prefix which is also a suffix
-- of the current input string
findFirst :: [(String, NAcceptor)] -> [String] -> NAcceptor
findFirst ((pfx, acc): pfxes) (sfx: sfxes)
	| pfx == sfx = acc
	| otherwise  = findFirst pfxes sfxes
findFirst _ _ = error "findFirst called with args of different length" -- should never happen 
