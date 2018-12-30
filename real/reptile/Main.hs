-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Main(main) where

import Mgrfuns
import Progfuns
import Auxprogfuns
import Layout
import Tilefuns

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
    replicateM_ 500 $ do
	fromMgr <- salt input
	let
	      toMgr = setmode 7 ++
		      shapewindow [0,0,1150,900] ++
		      setup ++
		      potatotile ([],1,initalist) (lines fromMgr) ++
		      shapewindow [0,0,500,500] ++
		      font 8 ++
		      textreset ++
		      clear ++
		      func 15
	print (hash toMgr)



