-- Main3.hs

-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991
-- with (map (AppendChan stdout) toMgr) *and* setup

module Main(main) where

import Mgrfuns
import Progfuns
import Auxprogfuns
import Layout
import Tilefuns

main :: [Response] -> [Request]
main ~(Str fromMgr : _) =
  (ReadChan stdin: map (AppendChan stdout) toMgr)
  where
  toMgr = [set, potatotile ([],1,initalist) (lines fromMgr),clearup]

set :: [Char]
set = setmode 7 ++
	shapewindow [0,0,1150,900] ++
	setup

clearup :: [Char]
clearup = shapewindow [0,0,500,500] ++
	  font 8 ++
	  textreset ++
	  clear ++
	  font 15



