module Main where

-- uses the IO monad
-- uses precompiled Delta table

import PreludeGlaIO
import DeltaG (final, qyfun, qnfun)

mainIO :: IO ()
mainIO = getArgsIO `thenIO` parse_args

parse_args :: [String] -> IO ()
parse_args (subject: files) =
	let acc = mkAcceptor subject
	    acc' = unlines . filter acc . lines
	in
	    readChanIO stdin `thenIO` \inp -> 
	    appendChanIO stdout (acc' inp) `thenIO_`
	    returnIO ()
parse_args _ = returnIO ()

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

-- findFirst implements the error function:
-- it returns the state after the longest consumed prefix which is also a suffix
-- of the current input string
findFirst :: [(String, NAcceptor)] -> [String] -> NAcceptor
findFirst ((pfx, acc): pfxes) (sfx: sfxes)
	| pfx == sfx = acc
	| otherwise  = findFirst pfxes sfxes
findFirst _ _ = error "findFirst called with args of different length" -- should never happen 
