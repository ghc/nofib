module Main where

import Parse
import Type
main          :: Dialogue
main          =  interact (linesP (reads :: (Parse String Type)))
