-- Time-stamp: <2008-10-21 13:26:36 simonmar>
-----------------------------------------------------------------------------

module Board where

import Wins

import Control.Parallel
import Control.Parallel.Strategies

type Board = [Row] 
type Row = [Piece]
data Piece = X | O | Empty deriving Eq

showBoard :: Board -> String
showBoard [r1,r2,r3] =  showRow r1 ++ "\n------\n" ++
			showRow r2 ++ "\n------\n" ++
			showRow r3 ++ "\n\n" 

showRow [p1,p2,p3] = showPiece p1 ++ "|" ++ showPiece p2 ++ "|" ++ showPiece p3


showPiece :: Piece -> String
showPiece X = "X"
showPiece O = "O"
showPiece Empty = " "

placePiece :: Piece -> Board -> (Int,Int) -> [Board]
placePiece p board pos | not (empty pos board) = []
placePiece p [r1,r2,r3] (1,x) = [[insert p r1 x,r2,r3]]
placePiece p [r1,r2,r3] (2,x) = [[r1,insert p r2 x,r3]]
placePiece p [r1,r2,r3] (3,x) = [[r1,r2,insert p r3 x]]

insert :: Piece -> Row -> Int -> Row
insert p [p1,p2,p3] 1 = [p,p2,p3]
insert p [p1,p2,p3] 2 = [p1,p,p3]
insert p [p1,p2,p3] 3 = [p1,p2,p]

empty :: (Int,Int) -> Board -> Bool
empty (1,x) [r1,r2,r3] = empty' x r1
empty (2,x) [r1,r2,r3] = empty' x r2
empty (3,x) [r1,r2,r3] = empty' x r3

empty' :: Int -> Row -> Bool
empty' 1 [Empty,_,_] = True
empty' 2 [_,Empty,_] = True
empty' 3 [_,_,Empty] = True
empty' _ _ = False

fullBoard b = and (parMap rnf notEmpty (concat b))
	where 
	notEmpty x = not (x==Empty)

--newPositions :: Piece -> Board -> [Board]
newPositions piece board = concat (parMap rwhnf (placePiece piece board) 
					     [(x,y) | x<-[1..3],y <-[1..3]])

initialBoard :: Board
initialBoard = [[Empty,Empty,Empty], 
		[Empty,Empty,Empty],
		[Empty,Empty,Empty]]

data Evaluation = XWin | OWin | Score Int deriving (Show,Eq)
{- OLD: partain
instance Eq Evaluation where
    XWin       == XWin	     = True
    OWin       == OWin	     = True
    (Score i1) == (Score i2) = i1 == i2
    _	       == _          = False
    a	  /= b	   = not (a == b)

instance Text Evaluation where
    showsPrec d XWin = showString "XWin"
    showsPrec d OWin = showString "OWin"
    showsPrec d (Score i) = showParen (d >= 10) showStr
	where
	  showStr = showString "Score" . showChar ' ' . showsPrec 10 i

    readsPrec p = error "no readsPrec for Evaluations"
    readList = error "no readList for Evaluations"
    showList []	= showString "[]"
    showList (x:xs)
		= showChar '[' . shows x . showl xs
		  where showl []     = showChar ']'
			showl (x:xs) = showChar ',' . shows x . showl xs
-}

eval 3 = XWin
eval (-3) = OWin
eval x = Score x

static :: Board -> Evaluation
static board = interpret 0 (parMap rwhnf (score board) wins)

interpret :: Int -> [Evaluation] -> Evaluation
interpret x [] = (Score x)
interpret x (Score y:l) = interpret (x+y) l
interpret x (XWin:l) = XWin
interpret x (OWin:l) = OWin

score :: Board -> Win -> Evaluation
score board win  = eval (sum (parMap rnf sum (zipWith (zipWith scorePiece) board win)))

scorePiece :: Piece -> Int -> Int
scorePiece X score = score
scorePiece Empty _ = 0
scorePiece O score = -score

{-
#if 0
-- This looks very much like a zipWith f to me
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f [] x = []
map2 f x [] = []
map2 f (x:xs) (y:ys) = f x y:map2 f xs ys
#endif
-}
