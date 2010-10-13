{----------------------------------------------------------
 --
 -- Local.hs : the information related to the evolved Map function
 -- T.Yu@cs.ucl.ac.uk   September 25, 1997
 --
 ----------------------------------------------------------}

module Local where
import Header (Expression(..),TypeExp(..),ExpressionList(..))
import Data.Array(Array, array)                
import Data.Char(ord)
--import Eval(atoi)
--import Auxil(indexL)

-- termEnv -----------------------------------------------------------------------------------
-- This is the terminal set for Map program

type Entry = (String, TypeExp)

termEnv :: Array Int Entry
termEnv = array(1,3) [ (1, ("nil", ListType (DummyType "a"))),
	    	       (2, ("l", ListType (GenType "G1"))),
		       (3, ("f", Brackets (Arrow (GenType "G1")(GenType "G2"))))]

-- funEnv -------------------------------------------------------------------------------
-- This is the function set for Map program

funEnv :: Array Int Entry
funEnv = array (1,7) [ (1, ("if-then-else", Arrow Boolean (Arrow (DummyType "a") 
				(Arrow (DummyType "a")(DummyType "a"))))), 
		       (2, ("map", Arrow  (Brackets (Arrow (GenType "G1")(GenType "G2"))) 
				(Arrow (ListType (GenType "G1"))(ListType (GenType "G2"))))),
		       (3, ("head", Arrow (ListType (DummyType "a")) (DummyType "a"))),
		       (4, ("tail", Arrow (ListType (DummyType "a")) (ListType (DummyType "a")))),
		       (5, ("f", Arrow (GenType "G1")(GenType "G2"))),
		       (6, ("cons", Arrow (DummyType "a") (Arrow (ListType (DummyType "a")) (ListType (DummyType "a"))))),
		       (7, ("null", Arrow (ListType (DummyType "a")) Boolean))]

-- runTimeErrorHandler---------------------------------------------------------------------------
-- This is the runTimeErrorHander set

runTimeErrorHandler :: [(TypeExp, Expression)]

runTimeErrorHandler= [(IntNum, (Constant "0")),
		      (Boolean, (Constant "false")),
		      (Str, (Constant " ")),
		      (TypeVar "x", Constant "0"), -- this is a problem
		      (GenType "x", Constant "0"),
		      (ListType (TypeVar "x"), (List [])),
		      (Brackets (Arrow (TypeVar "x")(TypeVar "y")),(Lambda "x" (Variable "x")))]

-- Mis parameters -----------------------------------------------------------------------------
--

constant = []
adfs = ["map"]
args = ["l","f"] -- lambda f. lambda l --
retType = (ListType (GenType "G2")) 
myName = "map"
maxScore = 100.00
printFitness = 500

-- test data---------------------------------------------------------------------
-- This is the test data for map program

testData :: ExpressionList

testData = [(Lambda "x" (Application (Primitive "aton") (Variable "x") IntNum)),(List [(Constant "A"),(Constant "B"),(Constant "C"),(Constant "D"),(Constant "E"),(Constant "F"),(Constant "G"),(Constant "H"),(Constant "I"),(Constant "J")]),(Lambda "x" (Application (Primitive "aton") (Variable "x") IntNum)), (List [])]

-- Expect Results ---------------------------------------------------------------------
-- This is the Expect Results

expectResults = [(List [(Constant "1"),(Constant "2"),(Constant "3"),(Constant "4"),(Constant "5"),(Constant "6"),(Constant "7"),(Constant "8"),(Constant "9"),(Constant "10")]),(List [])]

-- this is the fitness function for map only ---
--
-- S = -10 - 2 * length(L) -- rtError 
--     -10 - 2 * length(L) -- not halt
--     -2 * |length(L)-length(lr)| + sum ( 10 * 2 ** -dist(e,lr) )
--

evalFitness :: Expression -> Expression -> Bool -> Bool -> Double

evalFitness (List theExp) (List aExp) rtError halt = 
	let aLength = length aExp
	    theLength = length theExp
	    sum [] aExp = 0.0
	    sum (s@(Constant v):tl) aExp = 
		if s `notElem` aExp then
			sum tl aExp
		else
			 fromRational (10 / (2 ^ (abs ((indexL s aExp) - (atoi v))))) + sum tl aExp
	    sum any aExp = error "bad fitness value."
	    rtEval = if rtError then
			-10 - 2 * theLength
		     else
			0
	    haltEval = if halt then 
			0 
		       else
			-10 -2 * theLength
	in  --trace ("evalFitness : "++show aExp++show theExp ++show rtEval) $
	    fromInteger (toInteger (- (2 * abs (aLength - theLength)))) + sum theExp aExp + fromInteger (toInteger rtEval )+ fromInteger (toInteger haltEval)

-- otherwise there is bug in the program
evalFitness exp1 exp2 retError halt = -10000.00		

--the same code is in eval.hs. The extra copy avoids import eval.hs and solve
--compilation mutual dependency problem.
atoi :: String -> Int
atoi ('-':s) = value - value*2
		where value = atoi s
atoi s = foldl g 0 s
	 where 
	 g x y = (10 * x) + ((ord y) - 48)

--indexL function--

indexL item aList =
  case aList of
    {
	[] -> 0 ;
	(hd:tl) -> if item == hd then 1
		 	else ( 1 + indexL item tl)
    }
