{---------------------------------------------------------------
 --
 -- Main.hs : the main program for the PolyGP system.
 -- T.Yu@cs.ucl.ac.uk	September 25, 1997
 --
 --------------------------------------------------------------}

module Main where
import Header(Expression(..),TypeExp(..))
import Auxil (getParas, create, evolve, displayPop)
import System (getArgs)
import Random (randomInts,randomDoubles)

main = getArgs		>>=	\ [f1]		->
       readFile f1	>>= 	\ inputs	->
       let (treeDepth, popSize, randomInt, maxEval, parScale, xOverRate) = getParas inputs 0 0 0 0 0.0 0
	in if (treeDepth==0 || popSize==0 || randomInt==0 || maxEval==0 || parScale==0.0 || xOverRate==0) then
		print "Parameter reading fails."
	   else
	       let (population, rList) = create popSize [] (randomInts randomInt (randomInt + 10)) treeDepth
	   	   (population', dList, rList') = evolve population maxEval parScale popSize treeDepth xOverRate
				   (randomDoubles randomInt (randomInt+10)) rList
		in 
	    	   displayPop 1 population'



