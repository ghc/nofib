\documentclass[a4paper]{article}
\pagestyle{myheadings}
\usepackage{haskell}
\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Main}

This implements a complete multiple-matching clustering algorithm.

There are several output options, defined in different {\tt
main}-functions.  The most useful are probably

\begin{itemize}
\item {\tt main\_labels (-L)} print labels, one cluster on each line
\item {\tt main\_ug (-U)} output clusters in UniGene style
\item {\tt main\_matches (-M)} output each match (takes much less space)
\end{itemize}

\subsection{Imports}

Module declaration.  TODO: Haddockize it.

\begin{code}

module Main where
import Gene
import EST    (EST(..))
import Fasta  (parse, bmfparser, ugparser,FHParser,readDataLower)
import Suffix (Suffix(..), suffixesBy)
import Pairs  (pairs, matches, sort_io, sort_pure,
               show_header,show_regs,prefixes,merge_suffix_lists)
import Cluster(cluster_simple,cluster_all,
               newick,labels,unigene)    -- output
import Stats
import Clix   (clix)

import Data.Char   (isDigit)
import Data.List   (isPrefixOf)
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO     (hPutStrLn,stderr) -- for usage

type ParseF = Fasta.FHParser

\end{code}

\subsection{Main}

The {\tt main} function is just a simple wrapper.

\begin{code}

main :: IO ()
main = do
    -- read k, n and file names from args
    args <- getArgs
    let (opt,non,err) = getOpt Permute options args
    if err == [] && length non == 1 && validate opt
        then main_real1 (parseOpts opt) (head non)
        else usage err

\end{code}
\newpage
\subsection{The "real" main}

Where all the hard work is done...

\begin{code}

main_real1 :: Params -> String -> IO ()
main_real1 params file = do
    let (k,n) = (kval params,nval params)
    indata <- readFile file
    let es = parse (if upper_only params then ugparser else bmfparser) indata
    let sa = mkSufList (kval params) (match_only params) (preflen params) es
    let ms = matches sa
    let csizes = map (fromIntegral . length) sa
    let output = writer params
    -- make sure nothing needs be retained when matches are calculated
    case oflag params of
        Stats -> output ("PARAMETERS: k="++show k++", n="++show n
                   -- ++"\nSequences: "++ show (length es))
                   ++"\nCliques statistics:\n"
                   ++ (show $ Stats.uniVar csizes) ++"\n"
                   -- ++ (show $ Stats.quantiles csizes) ++ "\n"
                   )
        _ -> do
            let sort_fn = if extsort params
                          then sort_io es (file++"-"++show k ++
                               concatMap show (match_only params))
                          else (return . sort_pure)
            sms <- case collect params of
                       Just x -> merge_suffix_lists es x (file++"-"++show k)
                       Nothing -> sort_fn ms
            if match_only params /= [] then (sms `seq` return ())
              else do
                let ps = pairs (noLCR params) sms
                case oflag params of
                    Matches -> output (unlines $ map show sms)
                    Pairs   -> output $ unlines $ map show ps
                    PHead   -> output $ unlines $ map show_header ps
                    PRegs   -> output $ unlines $ map show_regs ps
                    Tree    -> output $ unlines $ map newick
                               $ cluster_all (addSgl params) es n ps
                    UG      -> output $ unlines $ map unigene
                               $ cluster_all (addSgl params) es n ps
                    Labels  -> output $ unlines $ filter (/="") $ map labels
                               $ cluster_simple (addSgl params) es n ps
                    _       -> error "Unsupported output parameter!\n"

\end{code}
\subsection{Helper functions}

A function (there used to be more, but what can I say?) useful for
{\tt main}

\begin{code}

-- | create the suffix list, with matching scores, by prefix
mkSufList    :: Int -> [Gene] -> Int -> [EST] -> [[Suffix]]
mkSufList k ms p ests = concatMap (clix k)
         [[concatMap (suffixesBy ss) ests] |
          ss <- filter (ms `isPrefixOf`) $ prefixes p]

\end{code}

\newpage
\subsection{Option handling}

Data structures and code for dealing with options (GetOpt style).
(Should we make the Params data structure a grammar only allowing
legal combinations of parameters?)

\begin{code}

data OFlag = UG | Labels | Stats | Pairs | Tree | Matches | PHead | PRegs
             deriving Eq

data Params = Params { kval, nval, preflen :: Int
                     , oflag      :: OFlag
                     , upper_only :: Bool
                     , extsort    :: Bool
                     , noLCR      :: Bool
                     , addSgl     :: Bool
                     , writer     :: String -> IO ()
                     , match_only :: [Gene]
                     , collect    :: Maybe Int
--            , ifile   :: String -- [FilePath]?
                     }

default_par = Params { preflen = 1, nval = 65, oflag = Labels
                     , extsort = False, upper_only = False
                     , match_only = [], collect = Nothing
                     , writer = putStr, noLCR = False, addSgl = False
                     , kval = 20
                     }

data Flag = KVal String
          | NVal String
          | PrefLen String
          | MatchOnly String
          | Collect String
          | InUG
          | AS
          | NoLCR
          | ExtSort
          | OutMatch | OutUG | OutLab | OutStats | OutPairs | OutRegs
          | OutTree | OutPHead | OutFile String deriving Show

validate :: [Flag] -> Bool
validate args =  and (map validate1 args) && validate2 args
    where validate1 arg = case arg of
                        KVal s -> (and $ map isDigit s)
                        NVal s -> (and $ map isDigit s)
                        PrefLen s -> (and $ map isDigit s)
                        MatchOnly s -> (and $ map (`elem` "ACGTacgt") s)
                        Collect s -> (and $ map isDigit s)
                        _ -> True
          validate2 _args = True -- todo: check if consistent

usage :: [String] -> IO ()
usage errs = hPutStrLn stderr (usageInfo (concat errs ++ usageString) options)

usageString :: String
usageString =
      "\nUsage: xsact -k K [-n N] [-p PL] [-x]" ++
      " [-M|-U|-L|-S|-P|-H|-N] [-u] [-i] [-s] [-o FILE] FILE\n" ++
      "Or for all PFXes: xsact -k K [-p PL] -x -m PFX FILE\n" ++
      "Followed by:      xsact -k K [-n N] -c PFXLEN -x" ++
      " [-M|-U|-L|-S|-P|-N] [-u] [-i] [-s] [-o FILE] FILE\n"

\end{code}
\newpage

\subsection{Option definitions}

Data for option definitions, as required by GetOpt.

\begin{code}

options :: [OptDescr Flag]
--           short long           result                description
options = [
    -- algorithm options
      Option ['k'] ["block-size"] (ReqArg KVal "INT")
      "Matching block size (default is 16)"
    , Option ['n'] ["threshold"]     (ReqArg NVal "INT")
      "Required clustering threshold"
    , Option ['p'] ["prefix-length"] (ReqArg PrefLen "INT")
      ("Prefix length defining suffix blocks "++
      "(increase to reduce memory footprint, decrease to increase speed)")
    , Option ['x'] ["external-sort"] (NoArg ExtSort)
      "use external sorting of matches, reducing memory footprint"
    -- input options
    , Option ['u'] ["input-upper"] (NoArg InUG)
      "accept only upper case characters"
    , Option ['m'] ["match-only"] (ReqArg MatchOnly "STR")
      "only generate matches starting with PREFIX"
    , Option ['c'] ["collect"] (ReqArg Collect "INT")
      "collect separately generated matches"
    , Option ['i'] ["dont-ignore-lcr"] (NoArg NoLCR)
      "don't ignore low complexity regions"
    -- output options
    , Option ['s'] ["add-singletons"] (NoArg AS)
      "include singleton clusters in the output"
    , Option ['M'] ["output-matches"] (NoArg OutMatch)
      "output all matches detected"
    , Option ['N'] ["output-newick"] (NoArg OutTree)
      "output a newick-formatted tree"
    , Option ['U'] ["output-unigene"] (NoArg OutUG)
      "output clustering UniGene style"
    , Option ['L'] ["output-labels"] (NoArg OutLab)
      "output clustering as a list of labels"
    , Option ['S'] ["output-statistics"] (NoArg OutStats)
      "output statistics"
    , Option ['P'] ["output-pairs"] (NoArg OutPairs)
      "output sequence pairs with match information (slow for large data sets)"
    , Option ['H'] ["output-headers"] (NoArg OutPHead)
      "output sequence pair headers (fast alternative to -P)"
    , Option ['R'] ["output-regions"] (NoArg OutRegs)
      "output matching regions for each sequence"
    -- output file
    , Option ['o'] ["output"] (ReqArg OutFile "FILE")
      "output file name (default is stdout)"
    ]

-- return values for k and n, and choices of main and output functions
parseOpts :: [Flag] -> Params
parseOpts = foldr po default_par
    where
    po f params = case f of
               OutFile fn  -> params {writer = writeFile fn}
               InUG        -> params {upper_only = True}
               NoLCR       -> params {noLCR = True}
               AS          -> params {addSgl = True}
               OutMatch    -> params {oflag = Matches}
               OutTree     -> params {oflag = Tree}
               OutUG       -> params {oflag = UG}
               OutLab      -> params {oflag = Labels}
               OutStats    -> params {oflag = Stats}
               OutPairs    -> params {oflag = Pairs}
               OutPHead    -> params {oflag = PHead}
               OutRegs     -> params {oflag = PRegs}
               KVal s      -> params {kval = read s}
               NVal s      -> params {nval = read s}
               PrefLen p   -> params {preflen = read p}
               ExtSort     -> params {extsort = True}
               MatchOnly s -> let p = params {match_only = readDataLower s,
                                              extsort = True}
                                  in if preflen p < length s
                                     then p { preflen = length s } else p
               Collect i -> params {collect = Just (read i)}

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\end{document}
