module Main
where

import Rsa
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
  replicateM_ 100 $ do
    input' <- salt input
    print (hash (encrypt 2036450659413645137870851576872812267542175329986469156678671505255564383842535488743101632280716717779536712424613501441720195827856504007305662157107
                         387784473137902876992546516170169092918207676456888779623592396031349415024943784869634893342729620092877891356118467738167515879252473323905128540213
                         input'))
