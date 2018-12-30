--
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
import	Pic
import  PicType	-- added by partain
import Control.Exception(evaluate)
import Control.Monad(replicateM_)
import Data.Char(ord)
import System.IO(hPutStr,stderr)
import System.Environment(getArgs)

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
    (n:_) <- getArgs
    replicateM_ (read n) $ do
		(_:s:_) <- getArgs
		evaluate (hash (takeWhile ((/=) '\n') s ++ pic (read s)))
