-- Time-stamp: <Fri Oct 20 1995 15:17:06 Stardate: [-31]6467.97 hwloidl>
-----------------------------------------------------------------------------

module Prog(prog) where

import Board
import Wins
import Game
import Tree

-- First arg decaffinates game
prog :: Int -> String -> String
prog decaf _ = showMove (head game)
	       --"OXO\n" ++
	       --concat (map showMove game)
	       where
	       game = if decaf == 0 
	                then error "Decaffination error\n"
			else alternate decaf X max' min' testBoard


testBoard = [[Empty,O,Empty],[Empty,X,Empty],[Empty,Empty,Empty]]

