-- Another puzzle search program. 
-- This one solves a solataire puzzle of the form
--		.
--	       * *
--	      * * *
--	     * * * *
--	    * * * * *
-- where '.' is the empty hole, and a move jumps
-- a peg '*' over another peg, removing the one
-- jumped over. The goal is to leave just one peg.

module Main where

import List
import Data.FiniteMap

---------------------------------------
-- Simple type declarations
---------------------------------------

-- The board starts with the topmost corner empty
-- The rows are numbered from the top starting at 1
-- and similarly columns

data Hole = E | F deriving( Eq, Ord )		-- Empty or full

type Row   = [Hole]
type Board = [Row]	-- First row has one hole, second has two, etc

type NRow = Int			-- Starts at 1
type NCol = Int			-- Starts at 1

type Move = (Pos,Pos)		-- From, to
type Pos  = (NRow, NCol) 

---------------------------------------
-- The Result type
---------------------------------------

-- We keep each result as a triple
-- The first component is a sorted list of moves, which we use
-- to eliminate equivalent results.  The second is a straightforward
-- list of moves (in reverse order), and the third is the final board state
type Result = (([Move], 	-- Sorted
		Board),		-- Final board
	       [Move]) 	-- In reverse order played

uniq :: [Result] -> [Result]	-- Eliminate equivalent results
uniq results = fmToList (listToFM results)
	-- Insert the results into a finite map, 
	-- keyed by (sorted-moves, board)
	-- This discards move sequences whose moves and result
	-- are the same (modulo ordering)

---------------------------------------
-- The main program 
---------------------------------------

main = run 5	-- 6 is too big; 4 has no solutions

run :: Int -> IO ()	
run size = putStr (showResults (movesN n_moves (initBoard size)))
	-- Initialise the board, run it for the required
	-- number of steps, and display the results
  where
    n_moves = ((size*(size+1)) `div` 2) - 3

-- (movesN n b) starts from board b and runs it 
-- for n steps, in all possible ways
movesN  :: Int 			-- Number of moves to run for
	-> Board		-- Initial board
	-> [Result]
movesN 0 b = [(([], b), [])]
movesN n b = uniq [ ((insert m sorted_ms, b''), m:ms) 
 		  | ((sorted_ms, b'), ms) <- movesN (n-1) b
		  , (m,  b'')  <- moves 1 b' ]


-- (moves r b) takes a partial board, starting at row r,
-- and returns all possible single moves
moves :: NRow -> Board -> [(Move, Board)]
moves nrow []     = []
moves nrow (r:rs) =  [(m,r':rs) | (m,r') <- hMoves nrow r]
		  ++ vMoves nrow (r:rs)
		  ++ [(m,r:rs') | (m,rs') <- moves (nrow+1) rs]

vMoves :: NRow -> Board -> [(Move, Board)]
vMoves nrow (rs1 : (r2:rs2): (r3a:r3b:rs3) : rss)	-- rs3 is longest
  =  go 1 1 [] rs1 []   (r2:rs2) []        (r3a:r3b:rs3)
  ++ go 1 3 [] rs1 [r2] rs2      [r3b,r3a] rs3
  where
    go :: NCol -> NCol
	-> [Hole] -> [Hole]
        -> [Hole] -> [Hole]
        -> [Hole] -> [Hole]
       -> [(Move,Board)]
    go ncol1 ncol3 rrs1 (F:rs1) rrs2 (F:rs2) rrs3 (E:rs3)
	= (((nrow,ncol1),(nrow+2,ncol3)), 
	   (rrs1 `glue` (E:rs1)) : (rrs2 `glue` (E:rs2)) : (rrs3 `glue` (F:rs3)) : rss)
	: go (ncol1+1) (ncol3+1) (F:rrs1) rs1 (F:rrs2) rs2 (E:rrs3) rs3

    go ncol1 ncol3 rrs1 (E:rs1) rrs2 (F:rs2) rrs3 (F:rs3)
	= (((nrow+2,ncol3),(nrow,ncol1)), 
	   (rrs1 `glue` (F:rs1)) : (rrs2 `glue` (E:rs2)) : (rrs3 `glue` (E:rs3)) : rss)
	: go (ncol1+1) (ncol3+1) (E:rrs1) rs1 (F:rrs2) rs2 (F:rrs3) rs3

    go ncol1 ncol3 rrs1 (r1:rs1) rrs2 (r2:rs2) rrs3 (r3:rs3)
	= go (ncol1+1) (ncol3+1) (r1:rrs1) rs1 (r2:rrs2) rs2 (r3:rrs3) rs3
    go ncol _ _ [] _ _ _ _ = []

vMoves nrow other = []

hMoves :: NRow -> Row -> [(Move, Row)]
hMoves nrow row
  = goH 1 [] row 
  where
    goH :: NCol -> [Hole] -> [Hole] -> [(Move,Row)]
    goH ncol rrs (F:F:E:rs) 
	= (((nrow,ncol), (nrow, ncol+2)), rrs `glue` (E:E:F:rs))
	: goH (ncol+2) (F:F:rrs) (E:rs)
    goH ncol rrs (E:F:F:rs) 
	= (((nrow,ncol+2), (nrow, ncol)), rrs `glue` (F:E:E:rs))
	: goH (ncol+1) (E:rrs) (F:F:rs)
    goH ncol rrs []     = []
    goH ncol rrs (r:rs) = goH (ncol+1) (r:rrs) rs

glue :: [Hole] -> [Hole] -> [Hole]	-- First arg is reversed
glue []      rs = rs
glue (r:rrs) rs = glue rrs (r:rs)


---------------------------------------
-- Initialisation and display code
---------------------------------------

initBoard :: Int -> Board
-- Start with the first forced move (3,1) -> (1,1)
initBoard n 
 | n >= 3 = [[F], [E,F], [E,F,F]] 
	   ++ take (n-3) [take i (repeat F) | i <- [4..]]
 | otherwise = error "Must have a board at least size 3"

showBoard :: Board -> String 
showBoard b
  = concatMap showRow ([n, n-1 ..] `zip` b)
  where
    n = length b

showRow (n,r) = take n (repeat ' ')
	      ++ concat [ [showHole h, ' '] | h <- r]
	      ++ ['\n']

instance Show Hole where
  show h = [showHole h]

showHole F = '#'
showHole E = '.'

showMove :: Move -> String
showMove (from,to) = (show from ++ "->" ++ show to)

showMoves :: Int -> [Move] -> String
showMoves i []     = "<no move>"
showMoves i [m]    = showMove m
showMoves i (m:ms) 
  | i < 4     = showMove m ++ ", " ++ showMoves (i+1) ms
  | otherwise = showMove m ++ ",\n" ++ showMoves 1    ms

showResults :: [Result] -> String
showResults as
  = concatMap show_one as ++ "\nTotal of " 
    ++ show (length as) ++ " solutions\n"
  where
    show_one ((_, b), ms) -- Add back in the first forced move
      = showMoves 1 (((3,1),(1,1)) : reverse ms) 
	++ "\n" ++ showBoard b ++ "\n"
