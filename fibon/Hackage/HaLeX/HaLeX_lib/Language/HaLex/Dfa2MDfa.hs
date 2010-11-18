--
-- From DFA into Monadic DFA in Haskell
--
-- Code Included in the Lecture Notes on
--
--      Language Processing (with a functional flavour)
--
--
-- copyright João Saraiva
--           Department of Computer Science,
--           University of Minho,
--           Braga, Portugal
--           jas@di.uminho.pt
--           2001
--

module Language.HaLex.Dfa2MDfa where

import Data.List
import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.FaOperations
import Language.HaLex.RegExp
import Language.HaLex.RegExp2Fa
--import Language.HaLex.RegExpParser
import Language.HaLex.Minimize

showAsAccumDfa  (Dfa v q s z delta) =
                showString ("dfa = Dfa v q s z delta") .
                showString ("\n  where \n\t v = ") .
                showList v .
                showString ("\n\t q = ") .
                showList q .
                showString ("\n\t s = ") .
                shows s    .
                showString ("\n\t z = ") .
                showList z .
                showString ("\n\t -- delta :: st -> sy -> m st \n") .
                showDfaMDelta q v delta .
                showString ("\n\n") .
                showString ("accum :: a -> State [a] () \n") .
                showString ("accum x = modify (\\ s -> s++[x])")

showDfaMDelta :: (Show st, Show sy) => [st] -> [sy] -> (st -> sy -> st) -> [Char] -> [Char]

showDfaMDelta q v d = foldr (.) (showChar '\n') f
  where
    f     = zipWith3 showF m n q'
    (m,n) = unzip l
    q'    = map (uncurry d) l
    l     = [(a,b) | a <- q , b <- v]

    showF st sy st' = showString("\t delta ") .
                      shows st .
                      showChar(' ') .
                      shows sy .
                      showString(" = do { accum ") .
                      shows sy .
                      showString (" ; return ") .
                      shows st' .
                      showString(" }") .
                      showChar('\n')



dfa2MIO :: (Show st , Show sy) => (Dfa st sy) -> IO ()
dfa2MIO afd = writeFile "GenMDfa.hs"
                        ("module GenMDfa where\n\n" ++
                         "import Language.HaLex.DfaMonad\n\n" ++
                         "import MonadState\n\n" ++
                          (showAsAccumDfa afd ""))


re2MHaskellMod re m b = "module GenMDfa where\n\n" ++
                        "import Language.HaLex.DfaMonad\n\n" ++
                         "import MonadState\n\n" ++
                        ((re2MDfa re m b) "")

re2MDfa :: (Show sy,Ord sy)
        => RegExp sy
        -> Bool       -- Minimized?
        -> Bool       -- Beautified?   (states as numbers)
        -> String -> String
re2MDfa re m b
  | m     && b     = showAsAccumDfa ((beautifyDfa . minimizeDfa . ndfa2dfa . regExp2Ndfa) re)
  | m     && not b = showAsAccumDfa ((minimizeDfa . ndfa2dfa . regExp2Ndfa) re)
  | not m && b     = showAsAccumDfa ((beautifyDfa . ndfa2dfa . regExp2Ndfa) re)
  | not m && not b = showAsAccumDfa ((ndfa2dfa . regExp2Ndfa) re)

{- Uses parser
re2MDfaIO :: [Char] -> IO ()
re2MDfaIO er      = dfa2MIO dfa
  where abst_er = f (parseRegExp er)
        dfa     = (beautifyDfa . minimizeDfa . ndfa2dfa . regExp2Ndfa) abst_er

re2MDfaIO' :: [Char] -> IO ()
re2MDfaIO' er     = dfa2MIO dfa
  where abst_er = f (parseRegExp er)
        dfa     = (beautifyDfa . ndfa2dfa . regExp2Ndfa) abst_er

re2MDfaIO'' :: [Char] -> IO ()
re2MDfaIO'' er    = dfa2MIO dfa
  where abst_er = f (parseRegExp er)
        dfa     = (ndfa2dfa . regExp2Ndfa) abst_er
-}


f (Just p ) = p
f _         = Epsilon






dfa_int = Dfa ['+','-','0','1']
              [1,2,3,4]
              1
              [3]
              delta
  where delta 1 '+' = 2
        delta 1 '-' = 2
        delta 1 '0' = 3
        delta 1 '1' = 3
        delta 2 '0' = 3
        delta 2 '1' = 3
        delta 3 '0' = 3
        delta 3 '1' = 3
        delta _  _  = 4






