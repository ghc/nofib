{---------------------------------------------------------------
 --
 -- Aux.hs : contains supporting function defination for the system.
 -- T.Yu@cs.ucl.ac.uk	September 25, 1997
 --
 --------------------------------------------------------------}

module Aux where
import Local(args,retType,testData,myName,expectResults,maxScore,evalFitness,printFitness)
import Header(TypeExp(..),ParseTree(..),Expression(..),Population(..))
import Create(createTree,extract)
import Evolve(mutateExp,xOverExp)
import Eval(evalExp,atoi,atof)
import Unify(applySubToExp,unify)
import Trace

-- getParas function -----------------------------------------------------------------------------
--

getParas :: String -> Int -> Int -> Int -> Int -> Double -> Int -> (Int, Int, Int, Int, Double, Int)

getParas inputs treeDepth popSize randomInt maxEval parScale xOverRate = 
  case inputs of
     {
	[] -> (treeDepth, popSize, randomInt, maxEval, parScale, xOverRate);

	_ ->
	let (current, rest)= nextWord inputs []
	    (value,rest') = nextWord rest []
	in if current == "treeDepth=" then 
		getParas rest' (atoi value) popSize randomInt maxEval parScale xOverRate
	   else if current == "popSize=" then 
		getParas rest' treeDepth (atoi value) randomInt maxEval parScale xOverRate
	   else if current == "randomInt=" then 
		getParas rest' treeDepth popSize (atoi value) maxEval parScale xOverRate
	   else if current == "maxEval=" then
		getParas rest' treeDepth popSize randomInt (atoi value) parScale xOverRate
	   else if current == "parScale=" then
		getParas rest' treeDepth popSize randomInt maxEval (atof value) xOverRate
	   else
		getParas rest' treeDepth popSize randomInt maxEval parScale (atoi value)
     }

-- nextWord --------------------------------------------------------------------------
--

nextWord :: String -> String -> (String,String)

nextWord [] word = error "Parameter inputs empty."
nextWord (x:xs) word = if x `elem` ['\n', '\r', ' '] then (word, xs) else nextWord xs (word ++ [x])

--create function-----------------------------------------------------------------------------------------------
-- 

--This function creates population with specified popSize. It checks to make sure that
-- every tree created is unique. Each individual is a lambada Expression.
--

create :: Int -> Population -> [Int] -> Int -> (Population,[Int])

create num pop rList treeDepth = 
--trace ("num is : " ++ show num) $
  case num of
    {
	0 -> trace ("create: top fitness is: "++show (snd (head pop))) $
	     (pop,rList);
	_ -> case (createTree treeDepth retType rList [] 1) of
	       { (aTree, rList', theta, typeNum') ->
		 let exp = applySubToExp (extract aTree) theta
	       	     createProgram exp args = 
			case args of
			{ [] -> exp;
		          (hdArg:tlArgs) -> createProgram (Lambda hdArg exp) tlArgs
			}
		     program = createProgram exp args
	   	 in
		     if ( aMem program pop) || (notExist args program) || (not (exist (Function myName) program )) 
		     then
			create num pop rList' treeDepth
		     else		
			if (num `mod` printFitness) == 0 && not (null pop) then
			   trace ("create: top fitness is: "++show (snd (head pop))) $
			   create (num-1) (inSort(getFitness (program, 0.0) [(myName,program)] args testData 
				expectResults) pop) rList' treeDepth 
		     	else
			   create (num-1) (inSort(getFitness (program, 0.0) [(myName,program)] args testData 
				expectResults) pop) rList' treeDepth
		}
     }

--exist function-----------------------------------------------------------------------
--	
exist :: Expression -> Expression -> Bool

exist e exp = case exp  of
	      {
		(Application exp1 exp2 t) ->
			if exist e exp1 then True
			else exist e exp2;
		(Lambda s exp) ->
			exist e exp;
		_ -> if e == exp then True else False
	      }

notExist [] program = False
notExist (first:rest) program = if not (exist (Variable first) program) then True
				else
				 notExist rest program

-- aMem function--------------------------------------------------------------------
--
				
aMem :: Expression -> Population -> Bool

aMem exp1 exp2 = case exp2 of
		 {
		  [] -> False;
		  ((aExp,fitness):rest) ->
			if exp1 == aExp then True
			else aMem exp1 rest
		 }

-- getFitness function---------------------------------------------------------------------
--
-- This function takes 5 arguments: the name of an expression, the expression and it's original
-- fitness value, argument name list and testData. It appends test data into expression before 
-- evaluation.

getFitness :: (Expression,Double) -> [(String,Expression)] -> [String] -> [Expression] -> [Expression] -> (Expression,Double)

getFitness (tree, fitness) adfs args [] expectResults = (tree, fitness)

getFitness (tree, fitness) adfs args testData expectResults =
  if fitness == 10000.00 then (tree, 20000.00) else  -- 10000 means bug in the evolved program
	let createProgram exp (last:[]) ((List aList):tlData) expectResults =  
		(Application exp (List aList) IntNum, tlData, length aList , 
		(head expectResults), (tail expectResults))
	    createProgram exp (hdArg:tlArgs) (hdData:tlData) expectResults = 
		createProgram (Application exp hdData IntNum) tlArgs tlData expectResults -- IntNum type is wrong
	    createProgram exp [] testData expectResults = error "No Argument variable is provided."
	    createProgram exp args [] expectResults = error "No test data is provided."
	    (program, testData', recursionLimit, theResult, expectResults') = 
		createProgram tree args testData expectResults
	    (aResult,rtError,halt,debug) = evalExp program adfs recursionLimit False True False
	in
	    --(tree,(aResult,rtError),0.0)
	    --trace ("getFitness : "++ show program ++ show testData'++show recursionLimit) $
	    if debug then
		(tree,10000.00)
	    else
	        getFitness (tree,(evalFitness theResult aResult rtError halt + fitness )) adfs args testData' expectResults'


-- inSort function ---------------------------------------------------------------
--

inSort :: (Expression,Double) -> Population -> Population

inSort exp [] = exp:[] 
inSort (exp1,fitness1)((exp2,fitness2):rest) =
	if fitness1 < fitness2	then
		(exp2,fitness2):inSort (exp1,fitness1) rest
	else
		(exp1,fitness1):((exp2,fitness2):rest)			

--evolve function --
-- steady-stead with 

evolve :: Population -> Int -> Double -> Int -> Int -> Int -> [Double] -> [Int] -> (Population,[Double],[Int]) 
evolve [] maxEval parScale popSize treeDepth xOverRate dList rList = error "Empty population."
 
evolve pop@((exp,fitness):rest) maxEval parScale popSize treeDepth xOverRate dList rList = 
   if fitness >= maxScore then 
	trace ("The perfect score in pop: "++show fitness++show exp)$
	(pop,dList,rList)
   else
   case maxEval of
	{ 0 -> (pop,dList,rList);
	  _ ->
	  let popSizeInReal = fromInteger (toInteger popSize) 
              selValue dList = ((head dList) * popSizeInReal * (parScale ^ popSize) * ( 1.0 - parScale ^ popSize) / 
			    (parScale ^ popSize * (1.0 - parScale)), tail dList)
              selIndex currVal randomVal | randomVal <= currVal = 0 -- 0-origin
					 | otherwise =  1 + selIndex (currVal*parScale) (randomVal - currVal)
	      getIndex aSeed = let i = selIndex popSizeInReal aSeed in if i < popSize then i else (popSize -1)
	      (seed1,dList') = selValue dList
      	      parent1 = pop !! (getIndex seed1)
      	      (seed2,dList'') = selValue dList'
	      (firstBorn,theta, rList') 
			= if (maxEval `mod` 1000 ) < xOverRate then
			  	xOverExp (fst parent1) (fst ( pop !! (getIndex seed2))) treeDepth treeDepth rList
			  else
			  	mutateExp (fst parent1) treeDepth treeDepth rList
  	   in
		if (aMem firstBorn pop) || (notExist args firstBorn) || (not (exist (Function myName) firstBorn )) 
		then
			evolve pop maxEval parScale popSize treeDepth xOverRate dList'' rList'
      		else
			let (child,fitness) = getFitness (firstBorn,0.00)
		   		   [(myName,firstBorn)] args testData expectResults
	    		    pop' = inSort (child,fitness) pop
	    		    pop'' = init pop'
  			in
			   if fitness >= maxScore then
				trace ("The number of evaluation done is the parameter maxEval - "++show maxEval++show "\n"++show parent1++show "\n"++show (pop !! (getIndex seed2))) $
				(pop'',dList'',rList')				
			    else if ((maxEval-1) `mod` printFitness) == 0 then
				trace ("evolve: top fitness is: "++show (snd (head pop))) $
			    	evolve pop'' (maxEval-1) parScale popSize treeDepth xOverRate dList'' rList'
			    else
				evolve pop'' (maxEval-1) parScale popSize treeDepth xOverRate dList'' rList'
	}

displayPop :: Int -> Population -> IO ()

displayPop num pop =
  case (num,pop) of
    {
	(_,[]) -> print "Population empty";

	(0,_) -> print "Done";

	(_,_ ) ->
		print (head pop) >>
		putChar '\n' >>
		displayPop (num - 1 ) (tail pop)
     }


--indexL function--

indexL item aList =
  case aList of
    {
	[] -> 0 ;
	(hd:tl) -> if item == hd then 1
		 	else ( 1 + indexL item tl)
    }
