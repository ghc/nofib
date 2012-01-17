{---------------------------------------------------------------
 --
 -- Evolve.hs : contains genetic operations (crossover and mutation)
 --		defination for the system.
 -- T.Yu@cs.ucl.ac.uk	September 25, 1997
 --
 --------------------------------------------------------------}

module Evolve (mutateExp,xOverExp) where
import Header(TypeExp(..), ParseTree(..),Expression(..))
import Create(createTree,extract)
import Unify(Theta(..),unify,xoverUnify,applySubToExp)
import Eval(atoi)


---mutateExp-------------------------------------------------------------------------------
--
-- This function takes a parse tree, a level and a randomList as arguments.
-- If the parse tree is successfully mutated, it returns the new parse tree.
-- Otherwise, it returns the orginal parse tree.
-- Mutation can't be performed at partial application node.

mutateExp::  Expression -> Int -> Int -> [Int] -> (Expression, Theta, [Int])

mutateExp anExp level treeDepth rList =
--  trace ("mutateExp "++show level) $
  case anExp of
    {
	(Lambda s exp) ->
		case (mutateExp exp level treeDepth rList) of
			{(exp', theta, rList') -> (Lambda s (applySubToExp exp' theta), theta, rList')};

	(Application exp1 exp2 aType@(Arrow t1 t2)) -> 
		case (mutateExp exp2 level treeDepth rList) of
		{ (exp2', theta, rList') ->
	  		if  exp2 == exp2' then
				case (mutateExp exp1 level treeDepth rList') of
	   			{ (exp1',theta',rList'') ->
		   		(Application exp1' exp2 aType, theta', rList'')
				}
	  		else
	     			(Application exp1 exp2' aType, theta, rList')
		};


	(Application exp1 exp2 aType) ->
		if (level /= treeDepth) && odd (head rList) then  -- we do not mutate at root level
			case (createTree level aType (tail rList) [] 20) of -- typeNum starts at 20
			{ (newTree, rList', theta, typeNum') ->
				if newTree == Empty then
					(anExp,[], rList')
			   	else 
					(extract newTree, theta, rList')
			}
		else -- no muation on this node, try subtrees
			case (mutateExp exp2 (level-1) treeDepth (tail rList)) of
			{ (exp2',theta,rList') -> 
	  			if exp2 == exp2' then -- no mutation happening
				case (mutateExp exp1 (level-1) treeDepth rList') of
	  			{ (exp1',theta',rList'') -> 
		   			(Application exp1' exp2 aType,theta',rList'')
				}
	  			else
	    			(Application exp1 exp2' aType,theta,rList')
		};

	_ -> (anExp, [], rList)
    }


-- xOverExp function----------------------------------------------------------------------------
--
-- This function takes two parse trees and performs crossover with them.
-- It returns one new prase tree if success or the first tree if not.

xOverExp :: Expression -> Expression -> Int -> Int -> [Int] -> (Expression,Theta,[Int])

xOverExp anExp tree2 level treeDepth rList = 
--  trace ("xOverExp "++show level) $
  case anExp of
    {
	(Lambda s exp) -> 
		--trace ("xOver lambda "++show exp++show level) $
		case (xOverExp exp tree2 level treeDepth rList) of
			{ (exp', theta, rList') -> (Lambda s (applySubToExp exp' theta), theta, rList')};

	(Application exp1 exp2 aType) ->
		--trace ("xOver App"++show aType++show tree2++show level) $
		if ( even (head rList) ) then -- we do xover at root level
			case (selectTree aType tree2 level treeDepth (tail rList)) of
			{ (newTree, theta, rList') -> 
				if newTree == Empty then
					(anExp,[], rList')
				else
					(extract newTree, theta, rList')
			}
  		else -- no xOver on this node, try subtrees, left to right
     			case (xOverExp exp2 tree2 (level-1) treeDepth (tail rList)) of

      			{ (exp2',theta,rList') ->
				if exp2 == exp2' then -- no xover happening
			   	case (xOverExp exp1 tree2 (level-1) treeDepth rList') of
			   	{ (exp1',theta',rList'') -> (Application exp1' exp2 aType,theta',rList'')}
				else
			   	(Application exp1 exp2' aType,theta,rList')
			};

	_ -> --trace ("xOver others"++show anExp++show level) $
	     (anExp, [], rList) -- don't do xover at leaf (constant, variable)
     }


--selectTree function-------------------------------------------------------------
--
--This function takes a Type and a Parse Tree. It select a branch in the Parse Tree
--which returns the same type as the given type. It either returns a new Tree or
--an Empty tree.
--Note: both aType and typeExp can contain type variable.	

selectTree :: TypeExp -> Expression -> Int -> Int -> [Int] -> (ParseTree, Theta, [Int])

selectTree aType anExp level treeDepth rList = 
  --trace ("selectTree "++show level) $
  case anExp of
     {
	(Lambda x exp) ->
		selectTree aType exp level treeDepth rList;

	(Application exp1 exp2 typeExp) ->
	-- trace ("selectTree"++show aType++show (Application exp1 exp2 typeExp)++show level) $
	if (level >= treeDepth && odd (head rList)) then
		let (unifiable,theta) = xoverUnify True [(aType, typeExp )][]
		in if unifiable then
			(ExpCons anExp, theta, tail rList)
		   else
			(Empty,[], tail rList)
	else -- did not select this node, try subtrees, from left to right
		case (selectTree aType exp2 (level+1) treeDepth (tail rList)) of
		{ (newBranch,theta,rList') ->
			if newBranch == Empty then 
			   selectTree aType exp1 (level+1) treeDepth rList'
			else
			   (newBranch,theta,rList')
		};

	_ -> (Empty, [], rList)
     }





