module Main where

import Parse
import Term
main          :: Dialogue
main          =  interact (linesP (reads :: (Parse String Term)))
