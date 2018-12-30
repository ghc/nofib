module Main(main) where
import Numbers
import Vectors
import Hide
import MyIO
import EdgePlate	( Input(..) )  -- partain
import Postscript	( Output(..) ) -- partain
import Control.Monad
import System.IO
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

main = do
  input <- hGetContents stdin
  replicateM_ 20 $ do
    ls <- salt input
    (getFilename $
      process (\viewdir -> hiddenline viewdir. map read. lines)) (lines ls)
