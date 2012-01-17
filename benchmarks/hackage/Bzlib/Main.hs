module Main(main) where

import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.BZip as BZip
import System.Environment


main = do
  fs <- getArgs
  mapM_ roundTrip fs

roundTrip f = do
  putStrLn f
  c <- B.readFile f
  B.writeFile (f ++ ".roundtrip") (process c)
    where process = (BZip.compress . BZip.decompress)
