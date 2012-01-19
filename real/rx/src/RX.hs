module Main (main) where


import FA (hsTNFA)

import Heave 
import Loop

import Gen
import Defaults


gmain :: String -> IO ()
-- give argument string, as you would on the command line
gmain args = gheave opts0 
	(expformat hsTNFA genval) (genpid, genenv) args

main :: IO ()
-- reads command line
main = heave opts0 
	(expformat hsTNFA genval) (genpid, genenv)

