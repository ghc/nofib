-- Time-stamp: <2008-10-21 13:21:24 simonmar>
-----------------------------------------------------------------------------

module Main where

import System.Environment
import Prog

main = do
  [n] <- fmap (map read) getArgs
  putStrLn (prog n)
