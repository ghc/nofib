{-# LANGUAGE TypeOperators #-}
import Data.Array.Parallel.Unlifted as U
import TestData
import System.IO
import System.Environment

dists :: [(String, Int -> U.Array (Double :*: Double))]
dists = [("square", genPointsUniform)
        ,("disc", genPointsDisc)]

main = do
         [sn,dist,file] <- getArgs
         let n   = read sn
             gen = case lookup dist dists of
                     Just f  -> f
                     Nothing -> error $ "Unknown distribution " ++ dist
             pts = gen n
         pts `seq` return ()
         h <- openBinaryFile file WriteMode
         U.hPut h pts
         hClose h

