module GHCmain where

import PreludeGlaST

mainPrimIO = _ccall_ puts "123\n"
