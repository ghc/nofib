\documentclass[a4paper]{article}
\pagestyle{myheadings}
\usepackage{haskell}
\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Split}

This implements a simple splitter for xsact's UniGene-formatted files.
Useful if you want to run an assembler on individual clusters.

\begin{code}

module Main where

import Util (break')

import System.Environment (getArgs)
import Data.List

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
           putStr usage
       else do
           inp  <- readFile (args!!0)
           writeIt (args!!0) 1 $ lines inp

usage :: String
usage = "Usage: split <file>\n\n"++
	"Splits the file whenever a line starts with a hash mark (#)\n"

writeIt :: String -> Integer -> [String] -> IO ()
writeIt prefix number stream = do
    if null stream then return ()
      else do
        let (this,next) = break' (\x -> (not.null) x && head x == '#') stream
        writeFile (prefix++"."++show number) $ unlines this
        writeIt prefix (number+1) next

\end{code}
\end{document}
