\documentclass[a4paper]{article}
\usepackage{haskell}
\pagestyle{myheadings}
\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Xtract}

This is the main module for {\tt xtract}, the eXperimental TRanscript
Assembly/Consensus Tool.  The idea is to construct the splice graph
from each EST cluster in the input, by first constructing and then
traversing the de Bruijn graph of $k-1$-words.

Hopefully, it will work as intended...

\begin{code}

module Main where

import Fasta (bmfparser, ugparser, parse, FHParser)
import WordMap (mkWordMap) -- , i2n)
import SpliceGraph (Path,greedy_paths,show_graph,path_sequence)
import EST(EST,label)
-- import Suffix(Dir(..))
import Util(breaks',foldl')

-- import Data.Set
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Char   (isDigit)

main :: IO ()
main = do
       args' <- getArgs
       let (opt,non,err) = getOpt Permute options args'
       if null err && length non == 1
          then do
               let args = foldl' (flip ($)) defaultArgs opt
               cont <- readFile (head non)
               let clusters = map unlines $ breaks'
                              (\x->(not . null) x && head x=='#') $ lines cont
               let es = map (parse (parser args)) clusters
               mapM_ (main_real args) $ zip [1..] es
          else usage err

data Output = G | C | X deriving (Read,Eq)
data Args   = Args { kval,wval :: Int,
                     output    :: Output,
                     writer    :: String -> IO (),
                     parser    :: Fasta.FHParser}

defaultArgs = Args
              { kval = usage ["You must specify a k value"]
              , wval = 0 -- heuristic below
              , output = usage ["Please specify -K, C, or X"]
              , writer = putStr, parser = bmfparser }

usage :: [String] -> a
usage errs = error (usageInfo (concat errs ++
    "\nUsage: xtract -k <kval> [-u] [-w] -{G|C|X} [-o FILE] <filename>\n")
    options)

-- add a k value to p
parseI :: String -> String -> Int
parseI err s = if (and $ map isDigit s) then read s
                 else usage [err]

options :: [OptDescr (Args -> Args)]
options =
    [Option ['k'] ["word-size"] (ReqArg
     (\s p -> p {kval = parseI "The word size (k-value) must be an integer" s})
     "INT") "Word size"
    ,Option ['w'] ["weight-minimum"] (ReqArg
     (\s p -> p {wval = parseI "The minimum weight must be an integer" s})
     "INT") "Ignore edges (words) weighing less than this"
    ,Option ['G'] ["graph-output"] (NoArg (\p -> p { output = G }))
     "Output the graph in GraphViz format"
    ,Option ['C'] ["consensus-output"] (NoArg (\p -> p { output = C }))
     "Output assembled consensus sequences"
    ,Option ['X'] ["exons-output"] (NoArg (\p -> p { output = X }))
     "Output the (concatenated) exons"
    ,Option ['u'] ["input-upper"] (NoArg (\p -> p { parser = ugparser }))
     "accept only upper case characters"
    ,Option ['o'] ["output"] (ReqArg (\s p -> p {writer = writeFile s}) "FILE")
     "output file name (default is stdout)"
    ]

-- the real stuff
main_real :: Args -> (Int,[EST]) -> IO ()
main_real args (n,es) = do
       -- construct word map
       let wm = mkWordMap (kval args) es
       -- traverse map, producing (writing?) exon sequences
       -- hPutStrLn stderr ("Seqs: "++show (length es) ++ ", Minw: "++show minw)
       let minw = if wval args > 0 then wval args
                  else ceiling (log (fromIntegral $ length es) / 2.0) -- heur.
       let paths = greedy_paths (wval args) wm
       let wfn = writer args
       case output args of
           C -> do wfn ("# Cluster no "++show n++" (k="
                        ++ show (kval args) ++ ", minw="++show minw++")\n"
                        ++ (unlines $ map
                        (\(i,x) -> mkfasta (mkhdr n i) x) $ zip [1..] paths))
           G -> do wfn (("// Cluster no "++show n) ++
                        (show_graph minw wm $ map trd paths))
           X -> error "Exon output not implemented yet"

mkhdr :: Int -> Int -> String
mkhdr n i = ">xtract-path-"++show n++"."++show i

mkfasta :: String -> (String,Int,Int,Path) -> String
mkfasta hdr (k,w,s,ps) = hdr ++ " /start="++k++" /weight="++show w
    ++ " /score="++show s ++"\n" ++ (concatMap show $ path_sequence ps)

trd (_,_,_,z) = z

showSeq es = "/sequences=" ++ (show $ map label es)

\end{code}
\end{document}
