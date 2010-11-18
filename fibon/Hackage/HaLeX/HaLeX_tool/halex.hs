--
-- The Main function of the halex tool
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



module Main where

import System.Console.GetOpt
import System.Environment
import System.IO
import Data.List (partition)

import Language.HaLex.RegExp
import Language.HaLex.RegExpParser
import Language.HaLex.RegExp2Fa
import Language.HaLex.RegExpAsDiGraph
import Language.HaLex.Equivalence

import Language.HaLex.Dfa
import Language.HaLex.Ndfa
import Language.HaLex.Minimize
import Language.HaLex.FaAsDiGraph
import Language.HaLex.Parser

import Language.HaLex.Dfa2MDfa

options     :: [OptDescr String]
options     =  [ Option ['N','n']
                 ["NDFA"]
                 (NoArg "N")
                 "generate Non-Deterministic Finite Automaton"
               , Option ['D','d']
                 ["DFA"]
                 (NoArg "D")
                 "generate Deterministic Finite Automaton"
               , Option ['M','m']
                 ["MinDfa"]
                 (NoArg "M")
                 "generate Minimized Deterministic Finite Automaton"
               , Option ['E','e']
                 ["Dfa with Effects"]
                 (NoArg "E")
                 "generate Reactive Deterministic Finite Automaton"
               ,  Option ['G','g']
                 ["graph"]
                 (NoArg "G")
                 "generate GraphViz input file"
               , Option ['S','s']
                 ["Sync State"]
                 (NoArg "S")
                 "include a Synk State In the Graph Representation"
               , Option ['R','r']
                 ["regular expression"]
                 (ReqArg ('r':) "string")
                 "specify regular expression"
               , Option ['o']
                 ["output"]
                 (ReqArg ('o':) "file")
                 "specify output file"
               , Option ['h','?']
                 ["help"]
                 (NoArg "h")
                 "output a brief help message"
               ]


main :: IO ()
main =
  do args <- getArgs
     let (o,n,errs) = getOpt Permute options args
     let (re,opts')  = partition ((=='r') . head) o
     let (fo,opts'') = partition ((=='o') . head) opts'
     let output = map tail re ++ repeat ""
     let opts = opts''
     if (errs /= []) || ("h" `elem` opts)
        then putStr $ usageInfo usageheader options
        else if (re == [] && n == [])
                then compileFromStdIn fo opts
                else compileFromFile re n fo opts
  where
    usageheader = "HaLex: Regular Languages in Haskell\n\n" ++
          "    Course project of Metodos de Programacao III (2001/2002)\n\n" ++
                  "Usage: halex options [file] ...\n\nList of options:"


type Options = [String]

compileFromFile :: [String] -> [String] -> [String] -> Options -> IO ()
compileFromFile re filei fileo opts
   = do let absREs :: [Maybe (RegExp Char)]
            absREs  = map (\x -> parseRegExp (tail x)) re
        let absREs' = map (\ (Just x) -> x)  absREs
        absREsFile <- mapM readRegExpFile filei
        let absREsFile' = map (\ (Just x) -> x)  absREsFile

        let absRE_all = absREs' ++ absREsFile'
--                  putStrLn (show absRE_all)

        if ((length absRE_all) == 1)
           then compileRegExp (head absRE_all) fileo opts
           else do let equiv = equivREs absRE_all
                   putStrLn (if equiv
                                then " The regular expressions are equivalent"
                                else " The regular expressions are not equivalent")


compileFromStdIn fileo opts
  = do (Just s) <- readRegExpFileHandle stdin
       compileRegExp s fileo opts

compileRegExp re fileo opts
  = do let fw = if (fileo == [])
                then if ("E" `elem` opts)
                     then writeFile "GenMDfa.hs"
                     else putStrLn
                else writeFile (tail (head fileo))
       if ("G" `elem` opts)
          then fw (re2graphviz re "HaLeX"
                                  (("D" `elem` opts) || not ("N" `elem` opts))
                                  ("M" `elem` opts) True ("S" `elem` opts))
          else if ("N" `elem` opts)
                 then do fw "import Ndfa"
                         fw (show (regExp2Ndfa re))
                 else if ("E" `elem` opts)
                      then do putStrLn " Generating file: GenMDfa "
                              fw (re2MHaskellMod re ("M" `elem` opts) True)
                      else do fw "import Dfa"
                              fw (show . beautifyDfa . minimizeDfa . regExp2Dfa $ re)


readRegExpFile :: FilePath -> IO (Maybe (RegExp Char))
readRegExpFile fp = do
    fh <- openFile fp ReadMode
    content <- hGetContents fh
    --hClose fh
    return (parseRegExp content)

readRegExpFileHandle :: Handle  -> IO (Maybe (RegExp Char))
readRegExpFileHandle fh = do
    content <- hGetContents fh
    return (parseRegExp content)

