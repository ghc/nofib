{---------------------------------------------------------------
 --
 -- Unify.hs : contains function definition of an unification
 --		algorithm
 -- T.Yu@cs.ucl.ac.uk	September 25, 1997
 --
 --------------------------------------------------------------}

module Unify(applySub,applySubToExp,unify,xoverUnify, Theta) where
import Header(TypeExp(..),Expression(..),ParseTree(..))
import Trace

-- type synonyms ------------------------------------------------------------------
--
-- Theta: a substituation set which binds type variables to type expression.

type Theta = [(String, TypeExp)]

-- replaceVar function ----------------------------------------------------------------
--
-- This function takes 2 arguments : a type variable string and theta.
-- It returns the TypeExp which the type variable is bound in
-- the theta. If the variable string is not bounded in the theta,
-- it return itself. (i.e. no replacement)
-- This function is called from applySub function.

replaceVar :: String -> Theta -> TypeExp

replaceVar var theta = case theta of
		       {
			[] -> TypeVar var;   -- no replacement
			((x,value):rest) -> if var == x then 
						value
				  	    else replaceVar var rest
			}

--replaceDummy function ----------------------------------------------------------------
--
-- This function takes two arguments :a dummy type variable string and theta.
-- It returns the TypeExp which the type variable is bound in
-- the theta.If the variable string is not bounded in the theta,
-- it return itself. (i.e. no replacement)
-- This function is called from applySub function.


replaceDummy :: String -> Theta -> TypeExp

replaceDummy var theta = case theta of
			{
			  [] -> DummyType var;   -- no replacement
			  ((x,value):rest) -> if var == x then 
						value
					      else replaceDummy var rest
			}

-- applySubToTree -------------------------------------------------------------------------
--
applySubToTree :: ParseTree -> Theta -> ParseTree

applySubToTree tree theta = case tree of 
			    {
				Empty -> tree ;
				(ExpCons exp) -> (ExpCons (applySubToExp exp theta))
			    }



-- applySubToExp -------------------------------------------------------------------------
--

applySubToExp :: Expression -> Theta -> Expression

applySubToExp exp theta = case exp of
			{
			  (Application exp1 exp2 typeExp) ->
				Application (applySubToExp exp1 theta)
				(applySubToExp exp2 theta)(applySub theta typeExp);
			  _ -> exp
			}

-- applySub function ---------------------------------------------------------------------
--
-- This function takes 2 arguments : theta and a type Expression.
-- It applies substituation by replacing all type variables appeared
-- in the Type Expression with new Term which is bound in theta.

applySub :: Theta -> TypeExp -> TypeExp

applySub theta typeExp = case typeExp of
			{
			 (TypeVar v) -> replaceVar v theta;
			 (DummyType v) -> replaceDummy v theta;
			 (ListType t) -> ListType (applySub theta t);
			 (Arrow t1 t2) -> Arrow (applySub theta t1) (applySub theta t2);
			 (Brackets e) -> Brackets (applySub theta e);
			 _ -> typeExp -- no substituation takes place
			}

-- substitute function --------------------------------------------------------------------
--
-- This function takes 3 arguments : a type variable, a new typeExp to substitute
-- the type variable and a type expression where substituation is
-- taking place. It returns the new type expression which substituation
-- has been done.
-- For every typeVar in TypeExp, replace it with newTerm

substitute :: String -> TypeExp -> TypeExp -> TypeExp

substitute typeVar newTerm typeExp = 

	case typeExp of
        {
	(TypeVar q)               -> if typeVar == q 
                                     then newTerm
		                     else 
			             typeExp; -- no substituation take place
	(ListType typeExp1)       -> ListType (substitute typeVar newTerm typeExp1);
	(Arrow typeExp1 typeExp2) -> Arrow (substitute typeVar newTerm typeExp1) 
					   (substitute typeVar newTerm typeExp2);
	(Brackets typeExp1)       -> Brackets (substitute typeVar newTerm typeExp1);
	_                         -> typeExp -- no substituation take place
        }

-- member function ----------------------------------------------------------------------------
--
-- This function takes two arguments: a variable and a Type Expression.
-- If the variable appears in the Type Expression, if returns Trues.
-- Otherwise, it returns False. This function is only used to check 
-- whether a temporary type variable exists in the type expression.
-- A dummy type variable can't never be unify with another dummy type.

member :: String -> TypeExp -> Bool

member var typeExp = case typeExp of
		    {
			(TypeVar tVar) -> if var == tVar then 
						True 
			    		  else False;
			(Arrow pType qType) -> member var pType || member var qType;
			(ListType lType) -> member var lType;
			(Brackets exp) -> member var exp;
			_ -> False  -- this takes care of DummyType
		     }

-- unify function ----------------------------------------------------------------------------------
--
-- This function takes a list of Type Expression Pairs, a Theta (binding of temporary type variables
-- or binding of dummy type varialbes). It unifies each pair in the list and returns a new Theta.
-- This function is used in two separate occasions: to bind dummy type variables in a polymorphic
-- function and to bind temporary type variables in the parse tree. The two occassions are handled
-- separately. Hence 2. & 4. can't never be used at the same time.

unify :: Bool -> [(TypeExp , TypeExp)] -> Theta -> (Bool, Theta)

unify unifiable typeExps theta =
  if not unifiable then
	(False, theta)
  else
	case typeExps of
	{
	 [] -> (True, theta);

	 --2. binding temporary type variables. can handle exp is temporary type variables.
	 ((s@(TypeVar v),exp):rest) ->
		if exp == s then 
			unify True rest theta 
		else if member v exp then
			(False,[])
		else 
       			let newRest = map (\(l, r) -> (substitute v exp l, substitute v exp r)) rest
		    	    newTheta = map (\(name, def) -> (name,substitute v exp def)) theta
		    	    {- substitute type variable V with Exp in the stack (rest) and in theta -}
			in
		    	    unify True newRest ((v,exp):newTheta); -- add v=exp to theta;
	 --3. in case s is instantiated through a dummy type variable and length(exp) > 1
	 ((exp, s@(TypeVar v)):rest) ->
		unify True ((s, exp):rest) theta;

	 --4. binding dummy type variables. exp can never be temporary/dummy type variables.
	 ((exp, s@(DummyType v)):rest) ->
		let newRest = map (\(l, r) -> (substitute v exp l, substitute v exp r)) rest
		    newTheta = map (\(name, def) -> (name,substitute v exp def)) theta
				{- substitute type variable V with Exp in the stack (rest) and in theta -}
		in
	    	    unify True newRest ((v,exp):newTheta); -- add v=exp to theta;

	 ((Arrow t1 t2, Arrow t3 t4):rest) ->
		unify True ((t1,t3):(t2,t4):rest) theta; -- this handle higher-order function as terminal

	 ((Brackets t1, Brackets t2):rest) ->
		unify True ((t1, t2):rest) theta;

	 ((ListType t1, ListType t2):rest) ->
		unify True ((t1,t2):rest) theta;

	 ((t1,t2):rest) -> 
		if t1 == t2 then 
			unify True rest theta
		else (False, [])
	}	

-- xoverUnify function -------------------------------------------------------------------
-- For xover nodes unification, Type variable can't unify with Function type.
-- Both application nodes and lambda nodes have to have the same number of 
-- args to be able to perform crossover.

xoverUnify :: Bool -> [(TypeExp, TypeExp)] -> Theta -> (Bool,Theta)

xoverUnify unifiable typeExps theta =
  if not unifiable then
	(False, theta)
  else
	case typeExps of
	{
	 [] -> (True, theta);

	 ((TypeVar v,Arrow t1 t2):rest) -> (False, theta);

	--2. binding temporary type variables. can handle exp is temporary type variables.
	 ((s@(TypeVar v),exp):rest) ->
		if exp == s then 
			xoverUnify True rest theta 
		else if member v exp then
			(False,[])
		else 
       			let newRest = map (\(l, r) -> (substitute v exp l, substitute v exp r)) rest
		    	    newTheta = map (\(name, def) -> (name,substitute v exp def)) theta
		    	    {- substitute type variable V with Exp in the stack (rest) and in theta -}
			in
		    	    xoverUnify True newRest ((v,exp):newTheta); -- add v=exp to theta;
	--3. in case s is instantiated through a dummy type variable and length(exp) > 1
	 ((exp, s@(TypeVar v)):rest) ->
		xoverUnify True ((s, exp):rest) theta;

	 ((Arrow t1 t2, Arrow t3 t4):rest) ->
		xoverUnify True ((t1,t3):(t2,t4):rest) theta; -- this handle higher-order function as terminal & crossover operation

	 ((Brackets t1, Brackets t2):rest) ->
		xoverUnify True ((t1, t2):rest) theta;

	 ((ListType t1, ListType t2):rest) ->
		xoverUnify True ((t1,t2):rest) theta;

	 ((t1,t2):rest) -> 
		if t1 == t2 then 
			xoverUnify True rest theta
		else 
			(False, [])
	}	




