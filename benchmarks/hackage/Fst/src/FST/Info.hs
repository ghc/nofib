{-
   **************************************************************
   * Filename      : Info.hs                                    *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : 89                                         *
   **************************************************************
-}

module FST.Info where

import FST.TransducerInterface

data Info = Info {
                  transducer :: (Transducer String,Bool),
		  expression :: (RReg String,Bool),
		  input      :: ([String],Bool),
		  outputs    :: ([String],Bool)
		  }

clearInfo :: Info -> Info
clearInfo info = info { transducer = (emptyTransducer,False),
                        expression = (empty,False),
                        input      = ([],False),
                        outputs    = ([],False) }

emptyInfo :: Info
emptyInfo = Info {
                   transducer = (emptyTransducer,False),
                   expression = (empty,False),
                   input      = ([],False),
                   outputs    = ([],False)
                 }

transducerBuilt :: Info -> Bool
transducerBuilt info = snd $ transducer info

expressionRead :: Info -> Bool
expressionRead info = snd $ expression info

inputRead :: Info -> Bool
inputRead info = snd $ input info

outputsRead :: Info -> Bool
outputsRead info = snd $ outputs info

updateTransducer :: Transducer String -> Info -> Info
updateTransducer t info = info {transducer = (t,True)}

updateExpression :: RReg String -> Info -> Info
updateExpression r info = info {expression = (r,True)}

updateInput :: [String] -> Info -> Info
updateInput inp info = info {input = (inp,True)}

updateOutputs :: [String] -> Info -> Info
updateOutputs out info = info { outputs = (out,True)}

getTransducer :: Info -> Transducer String
getTransducer = fst.transducer

getExpression :: Info -> RReg String
getExpression = fst.expression

getInput :: Info -> [String]
getInput = fst.input

getOutputs :: Info -> [String]
getOutputs = fst.outputs

noTransducer :: IO ()
noTransducer = do putStrLn "No transducer has been loaded/built."

noExpression :: IO ()
noExpression = do putStrLn "No regular expression has been typed/loaded into fstStudio."

noInput :: IO ()
noInput = do putStrLn "No input has been loaded into fstStudio."

noOutputs :: IO ()
noOutputs = do putStrLn "No outputs has been produced."

help :: IO ()
help = do putStrLn "\nList of Commands:"
          putStrLn "r <reg exp>    : read a regular relation from standard input."
	  putStrLn "b              : build a deterministic, minimal transducer."
	  putStrLn "bn             : build a possibly non-deterministic, non-minimal transducer."
	  putStrLn "m              : minimize loaded/built transducer."
          putStrLn "det            : determinize loaded/built transducer."
	  putStrLn "s  <filename>  : save to file."
	  putStrLn "l  <filename>  : load from file."
	  putStrLn "l a | b        : load and union."
	  putStrLn "l a b          : load and concatenate."
	  putStrLn "l a *          : load and apply Kleene's star."
	  putStrLn "l a .o. b      : load and compose."
	  putStrLn "vt             : view loaded/built transducer."
	  putStrLn "vr             : view typed/loaded regular relation."
          putStrLn "vi             : view loaded input."
          putStrLn "vo             : view produced output."
          putStrLn "d              : apply transducer down with loaded input."
          putStrLn "u              : apply transducer up with loaded input."
	  putStrLn "d <symbols>    : apply transducer down with symbols."
	  putStrLn "u <symbols>    : apply transducer up with symbols."
	  putStrLn "c              : Clear memory."
	  putStrLn "h              : display list of commands."
	  putStrLn "q              : end session.\n"

prompt :: IO ()
prompt = do putStr ">"

fstStudio :: IO ()
fstStudio = do putStrLn "\n*****************************************************"
	       putStrLn "* Welcome to Finite State Transducer Studio!        *"
	       putStrLn "* Written purely in Haskell.                        *"
	       putStrLn "* Version : 0.9                                     *"
	       putStrLn "* Date    : 11 August 2001                          *"
	       putStrLn "* Author  : Markus Forsberg                         *"
	       putStrLn "* Please send bug reports/suggestions to:           *"
	       putStrLn "* d97forma@dtek.chalmers.se                         *"
	       putStrLn "*****************************************************\n"
	       putStrLn "Type 'h' for help.\n"
