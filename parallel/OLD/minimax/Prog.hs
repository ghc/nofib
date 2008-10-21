-- Time-stamp: <2008-10-21 13:20:50 simonmar>
-----------------------------------------------------------------------------

module Prog(prog) where

import Board
import Wins
import Game
import Tree

-- First arg decaffinates game
prog :: Int -> String
prog decaf = showMove (head game)
	       --"OXO\n" ++
	       --concat (map showMove game)
	       where
	       game = if decaf == 0 
	                then error "Decaffination error\n"
			else alternate decaf X max' min' testBoard


testBoard = [[Empty,O,Empty],[Empty,X,Empty],[Empty,Empty,Empty]]

