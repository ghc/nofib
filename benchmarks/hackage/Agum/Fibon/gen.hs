module Main where
import Data.List ( sortBy )
import Data.Ord ( comparing )
import System.Random
import Control.Monad.State
import System.Environment

vars = ('a', 'z')
maxVars = 1000
maxMult = 9999 :: Int

main :: IO ()
main = do 
  [s,n] <- getArgs -- Seed,NumberOfEquations
  let gen = mkStdGen (read s)
      es  = evalState (eqns (read n)) gen
  putStr $ unlines es

eqns :: Int -> EqnState [String]
eqns cnt = sequence (replicate cnt eqn)

eqn :: EqnState String
eqn = do
  lhs <- formula
  rhs <- formula
  return $ lhs ++ "=" ++ rhs

formula :: EqnState String
formula = do
  g <- get
  let (n, g') = randomR (1, maxVars) g
  put g'
  vs <- sequence $ replicate n     factor
  fs <- sequence $ replicate (n-1) function
  return $ concat (interleave vs fs)

type EqnState a = State (StdGen) a

factor :: EqnState String
factor = do
  g <- get
  let (v, g')  = randomR (1, maxVars) g --randomR vars         g
      (m, g'') = randomR (1, maxMult) g'
  put g''
  return $ (show m) ++ "x" ++ show v

function :: EqnState String
function = do
  g <- get
  let (b, g') = randomR (True, False) g
  put g'
  return $ if b then "+" else "-"

interleave :: [a] -> [a] -> [a]
interleave []   _ = []
interleave [x]  _ = [x]
interleave xs  [] = xs
interleave (x:xs) (y:ys) = [x,y] ++ interleave xs ys


