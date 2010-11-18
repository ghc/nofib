import qualified Types as QH
import QuickHullVect (quickhull)

import Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Prelude
import qualified Data.Array.Parallel.Prelude.Double as D
import Data.Array.Parallel.PArray as P
import Data.Array.Parallel
import Data.Array.Parallel.Prelude ( fromUArrPA_2' )

import Prelude as Prel
import qualified System.Random as R
import System.IO
import Control.Exception (evaluate)

import Bench.Benchmark
import Bench.Options
--import TestData


-- Random points generation
--

-- IMPORTANT: We use the same seed with the same random generator in all
--            quickhull codes.  The asymptotic work complexity of quickhull
--            is between O (N) and O (N^2) depending on the input.
--            To compare benchmark results, they always need to use the same
--            input.

generatePoints :: Int -> Maybe String -> IO (Point (PArray QH.Point))
generatePoints n s
  = do
      let rg = R.mkStdGen 42742     -- always use the same seed
          ds = U.randomRs (n*2) (-100, 100) rg
          xs = U.extract ds 0 n
          ys = U.extract ds n n
      save s xs ys
      convert xs ys
  where
    save Nothing  _ _ = return ()
    save (Just s) xs ys = do
                            h <- openBinaryFile s WriteMode
                            U.hPut h (U.zip xs ys)
                            hClose h

{-
          ps  = toPairs (take (2*n) (R.randomRs (-100, 100) rg))
          pts = QH.points (P.fromList (Prel.map fst ps))
                          (P.fromList (Prel.map snd ps))
      evaluate $ nf pts -- force pts
      return $ ("N = " ++ show n) `mkPoint` pts
  where
    toPairs []        = []
    toPairs (x:y:pts) = (x, y) : toPairs pts

    force pts = toUArrPA (QH.xsOf pts) U.!: 0 D.+ 
                toUArrPA (QH.ysOf pts) U.!: 0
-}

loadPoints :: String -> IO (Point (PArray QH.Point))
loadPoints file
  = do
      h <- openBinaryFile file ReadMode
      upts <- U.hGet h
      hClose h
      convert (U.fsts upts) (U.snds upts)
{-
      let pts = QH.points (fromUArrPA' (U.fsts upts)) (fromUArrPA' (U.snds upts))
      evaluate $ nf pts
      return $ ("N = " ++ show (U.length upts)) `mkPoint` pts
-}

convert :: U.Array Double -> U.Array Double -> IO (Point (PArray QH.Point))
convert xs ys
  = do
      let pts = QH.points (fromUArrPA' xs) (fromUArrPA' ys)
      evaluate $ nf pts
      return $ ("N = " ++ show (U.length xs)) `mkPoint` pts



-- Main
-- ----

{- Simple test
pts = points (P.fromList (Prel.map fst coords))
             (P.fromList (Prel.map snd coords))
  where
    coords = [(3,3),(2,7),(0,0),(8,5), (4,6),(5,3),(9,6),(10,0)]

result = Prel.zip (U.toList (toUArrPA (xsOf ps)))
                  (U.toList (toUArrPA (ysOf ps)))
  where
    ps = quickhull pts

main = print result
 -}

main = ndpMain "Quick hull"
               "[OPTION] ... SIZES ..."
               run [] ()

run opts () [] = failWith ["No sizes or input files specified"]
run opts () args =
  do
    benchmark opts quickhull
        (gen args)
        nf
        (\ps -> "Result length = " ++ show (P.length ps))
    putStrLn "OK"
    return ()
  where
    gen [] = []
    gen (arg:args)
      = case reads arg of
          [(n,"")] -> case args of
                        ("w" : s : args') -> generatePoints n (Just s) : gen args'
                        _                  -> generatePoints n Nothing  : gen args
          _        -> loadPoints arg : gen args

{-
  case Prel.map read sizes of
    []  -> failWith ["No sizes or input files specified"]
    szs -> do
             benchmark opts runQuickhull
                (Prel.map generatePoints szs)
                (`seq` ()) (("Result length = " ++) . show)
             return ()
  where
    runQuickhull :: PArray QH.Point -> Int
    runQuickhull pts = let result = quickhull pts
                           resxs  = toUArrPA (QH.xsOf result)
                       in
                       resxs U.!: 0 `seq` U.length resxs
-}
        
