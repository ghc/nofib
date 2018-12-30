module Main where

import Parse
import Shows
import Term
import Type
import Environment
import InferMonad
import Substitution	( Sub )
import MaybeM		( Maybe )
import Infer

import Control.Monad
import Data.Char
import System.Environment

-- | Using @salt xs@ on an loop-invariant @xs@ inside a loop prevents the
-- compiler from floating out the input parameter.
salt :: [a] -> IO [a]
salt xs = do
  s <- length <$> getArgs
  -- Invariant: There are less than 'maxBound' parameters passed to the
  --            executable, otherwise this isn't really @pure . id@
  --            anymore.
  pure (take (max (maxBound - 1) s) xs)

hash :: String -> Int
hash = foldr (\c acc -> ord c + acc*31) 0

main = do
  input <- getContents
  replicateM_ 200 $ do
    input' <- salt input
    let inferred = concat (map readInferShow (lines input'))
    print (hash (showsString (show testEnv ++ prompt) inferred))

readInferShow :: String -> String
readInferShow =  useP ("Failed to parse" ++ prompt)   (
                 lexactlyP reads                      `eachP` (\t ->
                 useI ("Failed to type" ++ prompt)    (
                 inferTerm testEnv t                  `eachI` (\tt ->
                 show t ++ " : " ++ show tt ++ prompt))))
testEnv       :: Env
testEnv       =  read
                   (   "[ unit   : x -> List x,"
                   ++  "  append : List x -> List x -> List x,"
                   ++  "  fix    : (x -> x) -> x ]"
                   )
prompt        :: String
prompt        =  "\n? "
