import SumSqPrim ( sumSq )

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R

import qualified Data.Array.Parallel.Unlifted as U

import Bench.Benchmark
import Bench.Options

main = ndpMain "Sum squares"
               "[OPTION] ... SIZES ..."
               run [] ()

run opts () sizes =
  case map read sizes of
    []  -> failWith ["No sizes specified"]
    szs -> do
             benchmark opts sumSq
                (map (\n -> return $ ("N = " ++ show n) `mkPoint` n) szs)
                (`seq` ()) show
             return ()

