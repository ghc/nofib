{---------------------------------------------------------------
 --
 -- Eval.hs : contains function definition which evaluate/interpret
 --	      the genetic programs evolved by the system.
 -- T.Yu@cs.ucl.ac.uk	September 25, 1997
 --
 --------------------------------------------------------------}

module Eval (evalExp,atoi,atof) where
import Header(TypeExp(..), Expression(..))
import Local(runTimeErrorHandler)
import Char(ord)
import Trace



--evalExp function---------------------------------------------------------------
--
-- This function takes 3 arguments : an expression, an adf list and counter.
-- It evaluates the expression and return its result, an expression. The counter
-- is used to check for recursive calls.

evalExp:: Expression -> [(String,Expression)] -> Int -> Bool -> Bool -> Bool -> (Expression,Bool,Bool,Bool)

evalExp exp adfs counter rtError halt debug = 
  case exp of
    {
	(Constant x) -> (exp, rtError, halt,debug);
	(List x) -> (exp, rtError, halt,debug);
	(Variable x) -> (exp, rtError, halt,debug);
	(Primitive x) -> (exp, rtError, halt,debug);
	(Function x) -> (exp, rtError, halt ,debug);
	(Lambda x e) -> (exp, rtError, halt,debug);

	-- head,tail,null (strict)

	(Application (Primitive f) arg t) ->
	 --trace ("Primitive1 : "++ show f ++ show arg ) $ 
  		if (not halt) || debug then
			(errorHandler t, rtError, halt, debug)
		else
			doPrim1 f arg t adfs counter rtError True False;

	--+,-,==,cons(non-strict)

	(Application (Application (Primitive f) arg1 t1) arg2 t2) ->
	--trace ("Primitive2 : "++ show f ++ show arg1 ++ show arg2) $ 
		if (not halt) || debug then
			(errorHandler t2, rtError, halt, debug)
		else
			doPrim2 f arg1 arg2 t2 adfs counter rtError True False;

	--if-then-else (1st arg strict)

	(Application (Application (Application (Primitive f) arg1 t1 ) arg2 t2 ) arg3 t3) ->
	--trace ("Primitive3 : "++ show f ++ show arg1 ++ show arg2 ++ show arg3) $ 
  		if (not halt) || debug then
			(errorHandler t3, rtError, halt, debug)
  		else
			doPrim3 f arg1 arg2 arg3 t3 adfs counter rtError True False;

	--applicative-order reduction

	(Application (Lambda x e) y t) ->
	--trace ("Lambda "++ show x ++ show e ++ show y) $ 
	if (not halt) || debug then
		(errorHandler t, rtError, halt, debug)
	else
		case (evalExp y adfs counter rtError True False) of 
		 { (y', rtError', halt',debug') -> 
			if (not halt') || debug' then
			  (errorHandler t,rtError',True, False)
		  	else
			  evalExp (betaReduction y' x e) adfs counter rtError' True False
		};

	--normal-order reduction (app (app (lambda f.lambda l e) aF) aL)

	(Application (Application (Lambda f e) aF t1) aL t2) ->
	--trace ("Lambda "++ show f ++ show e ++ show aF ++ show aL) $ 
	if (not halt) || debug then
		(errorHandler t2 ,rtError, halt, debug)
	else	
		case (evalExp aF adfs counter rtError True False) of
		{ (aF',rtError', halt',debug') ->
		  if (not halt') || debug' then
			(errorHandler t2, rtError', halt',debug')
		  else
			evalExp (Application (betaReduction aF' f e) aL t2) adfs counter rtError' True False
		};

	--make a copy of the adf expression

	(Application (Function x) e t) ->
	--trace ("Function "++ show x ++ show e ) $ 
	if (counter <= 0) || (not halt)  then 
		(errorHandler t,rtError, False, debug) -- map return type is ListType TypeVar T2
	else if debug then
		(errorHandler t,rtError, True, True) -- map return type is ListType TypeVar T2
	else
		evalExp (Application (lookUp x adfs) e t) adfs (counter-1) rtError True False;

	--make a copy of the adf expression

	(Application (Application (Function x) e1 t1) e2 t2) ->
	--trace ("Function "++ show x ++ show e1 ++ show e2) $ 
	if (counter <= 0) || not halt then 
		(errorHandler t2, rtError, False, debug) -- map return type is ListType TypeVar T2
	else if debug then
		(errorHandler t2, rtError, True, True)
	else
		evalExp (Application (Application (lookUp x adfs) e1 t1) e2 t2) adfs (counter-1) rtError True False;

	(Application f e t) ->  --** a DEBUGGING statement--
		trace ("evalExp:  "++ show exp ) $
		(errorHandler t, rtError, halt, True)
	}

--doPrim1 function--------------------------------------------------------------------------------------------
--
-- This function takes 6 arguments : name of the primitive function, expression to be evaluated ,
-- return type of the expression, adf list, counter and rtError flag.
-- It evaluates the expression and returns the result.
-- Note: only when the rtError is False that is function can be envoked. evalExp checks this.

doPrim1 :: String -> Expression -> TypeExp -> [(String, Expression)] -> Int -> Bool -> Bool -> Bool -> (Expression,Bool,Bool,Bool)

doPrim1 funName exp aType adfs counter rtError halt debug = 
	case funName of
	{
	  "head" ->
		--trace ("doPrim1 head :"++show exp) $
		case (evalExp exp adfs counter rtError halt debug) of
		{ (List aList, rtError', halt',debug') ->
	   	  if (not halt') || debug' then
			(errorHandler aType, rtError', halt', debug')
	   	  else
			if null aList then -- head of [] is a run-time error
		  	(errorHandler aType, True, True, False)
	   		else 
		  	evalExp (head aList) adfs counter rtError' True False;
		  (exp', rtError', halt', debug') -> --** a DEBUGGING statement--
			trace ("head:  "++ show exp ) $
			(errorHandler aType, rtError', halt', True)
		};

	  "tail" ->
		--trace ("doPrim1 tail :"++show exp) $
		case (evalExp exp adfs counter rtError halt debug) of
		{ (List aList,rtError', halt',debug') ->
	   	  if (not halt') || debug' then
			(errorHandler aType, rtError', halt', debug')
	   	  else
			if null aList then 
		   		(List [],True,True, False)  -- tail of [] is []. ?? wrong ??
			else 
		   		evalExp (List (tail aList)) adfs counter rtError' True False;
		  (exp', rtError', halt', debug') -> --** a DEBUGGING statement--
			trace ("tail:  "++ show exp ) $
			(errorHandler aType, rtError', halt', True)	
		};

	"null" ->
		--trace ("doPrim1 null :"++show exp) $
		case (evalExp exp adfs counter rtError halt debug) of
		{ (List aList,rtError', halt',debug') ->
	  	  if (not halt') || debug' then
			(Constant "false",rtError', halt', debug')
		  else
		  	if (null aList) then 
				(Constant "true",rtError',True, False)
	  	  	else (Constant "false",rtError', True,False);
		  (exp', rtError', halt', debug') -> --** a DEBUGGING statement--
			trace ("null:  "++ show exp ) $
			(errorHandler aType, rtError', halt', True)
		};

	"length" ->
		--trace ("doPrim1 length :"++show exp) $
		case (evalExp exp adfs counter rtError halt debug) of
		{ (List aList,rtError', halt',debug') ->
	  	  if (not halt') || debug' then
			(Constant "0",rtError', halt', debug')
		  else
	  	  	(Constant (show (length aList)),rtError', True,False);

		  (exp', rtError', halt', debug') -> --** a DEBUGGING statement--
			trace ("length:  "++ show exp ) $
			(errorHandler aType, rtError', halt', True)
		};

	"aton" ->
		--trace ("doPrim1 aton :"++show exp) $
		case (evalExp exp adfs counter rtError halt debug) of
		{ (Constant alphabet,rtError', halt',debug') ->
	  	  if (not halt') || debug' then
			(Constant "0",rtError', halt', debug')
		  else
	  	  	(Constant (show ((ord (head alphabet))-64)) ,rtError', True,False);

		  (exp', rtError', halt', debug') -> --** a DEBUGGING statement--
			trace ("aton:  "++ show exp ) $
			(errorHandler aType, rtError', halt', True)
		};

	_ ->  --** a DEBUGGING statement--
  	trace ("doPrim1:  "++ show funName ++ show exp) $
	(errorHandler aType, rtError, halt, True)
	}

-- doPrim2 function ---------------------------------------------------------------------------
-- This is the doPrim2 function

doPrim2 :: String -> Expression -> Expression -> TypeExp -> [(String,Expression)] -> Int -> Bool -> Bool -> Bool -> (Expression, Bool,Bool,Bool)

doPrim2 funName e1 e2 t adfs counter rtError halt debug = 
  case funName of
  {
	"+" ->
	--trace ("doPrim2 + :"++show e1 ++ show e2) $
	let (Constant a,rtError',halt',debug') = evalExp e1 adfs counter rtError halt debug
	    (Constant b,rtError'', halt'',debug'') = evalExp e2 adfs counter rtError' halt' debug'
	in
	    if (not halt'') || debug'' then
		(Constant "0", rtError'', halt'', debug'')
	    else
	    	(Constant (show ( (atoi a)+ (atoi b))), rtError'', True, False);

	"-" ->
	--trace ("doPrim2 - :"++show e1 ++ show e2) $
	let (Constant a, rtError', halt',debug') = evalExp e1 adfs counter rtError halt debug
	    (Constant b, rtError'', halt'',debug'') = evalExp e2 adfs counter rtError' halt' debug'
	in
	    if (not halt'') || debug'' then
		(Constant "0", rtError'', halt'', debug'')
	    else
	    	(Constant (show ((atoi a)- (atoi b))),rtError'', True, False);
	"*" ->
	--trace ("doPrim2 + :"++show e1 ++ show e2) $
	let (Constant a,rtError',halt',debug') = evalExp e1 adfs counter rtError halt debug
	    (Constant b,rtError'', halt'',debug'') = evalExp e2 adfs counter rtError' halt' debug'
	in
	    if (not halt'') || debug'' then
		(Constant "0", rtError'', halt'', debug'')
	    else
	    	(Constant (show ( (atoi a) * (atoi b))), rtError'', True, False);

	"==" ->
	--trace ("doPrim2 == :"++show e1 ++ show e2) $
	let (e1', rtError',halt',debug') = evalExp e1 adfs counter rtError halt debug
	    (e2', rtError'', halt'',debug'') = evalExp e2 adfs counter rtError' halt' debug'
	in
	    if (not halt'') || debug'' then
		(Constant "false", rtError'', halt'', debug'')
	    else
	    case (e1',e2') of
	    {
	    (Constant a, Constant b) ->
		if ((atoi a) == (atoi b)) == True then
			(Constant "true", rtError'', True, False)
 	    	else
	    		(Constant "false", rtError'', True, False);
	     _ -> trace ("== : " ++ show e1 ++ show e1' ++ show e2 ++show e2') $
		  error ("==  error \n")
	    };

	"<=" ->
	--trace ("doPrim2 <= :"++show e1 ++ show e2) $
	let (e1', rtError',halt',debug') = evalExp e1 adfs counter rtError halt debug
	    (e2', rtError'', halt'',debug'') = evalExp e2 adfs counter rtError' halt' debug'
	in
	    if (not halt'') || debug'' then
		(Constant "false", rtError'', halt'', debug'')
	    else
	    case (e1',e2') of
	    {
	    (Constant a, Constant b) ->
		if ((atoi a) <= (atoi b)) == True then
			(Constant "true", rtError'', True, False)
 	    	else
	    		(Constant "false", rtError'', True, False);
	     _ -> trace ("<= : " ++ show e1 ++ show e1' ++ show e2 ++show e2') $
		  error ("<=  error \n")
	    };

	">" ->
	--trace ("doPrim2 > :"++show e1 ++ show e2) $
	let (e1', rtError',halt',debug') = evalExp e1 adfs counter rtError halt debug
	    (e2', rtError'', halt'',debug'') = evalExp e2 adfs counter rtError' halt' debug'
	in
	    if (not halt'') || debug'' then
		(Constant "false", rtError'', halt'', debug'')
	    else
	    case (e1',e2') of
	    {
	    (Constant a, Constant b) ->
		if ((atoi a) > (atoi b)) == True then
			(Constant "true", rtError'', True, False)
 	    	else
	    		(Constant "false", rtError'', True, False);
	     _ -> trace ("> : " ++ show e1 ++ show e1' ++ show e2 ++show e2') $
		  error (">  error \n")
	    };

	--both x and exp are strict
	"cons" ->
	--trace ("doPrim2 cons:"++ show e1 ++ show e2) $
	case (evalExp e1 adfs counter rtError halt debug) of
	{ (e1',rtError', halt',debug') ->
	   if (not halt') || debug' then
		(List [],rtError', halt', debug')
	   else
		case (evalExp e2 adfs counter rtError' True False) of
 		{ (List e2',rtError'',halt'',debug'') ->
		   if (not halt'') || debug'' then
			(List [], rtError'', halt'', debug'')
		   else
			(List (e1':e2'), rtError'', True, False);
		  (e2', rtError'', halt'', debug'') -> --** a DEBUGGING statement--
		  	trace ("cons:  "++ show e1++show e2) $
			(errorHandler t , rtError'', halt'', True)
		}
	};

	_ -> --** a DEBUGGING statement--
	trace ("doPrim2: "++ show funName ++ show e1 ++ show e2) $
	(errorHandler t , rtError, halt, True)
	}
	
-- atoi function --------------------------------------------------------------------------------
--
atoi :: String -> Int
atoi ('-':s) = value - value*2
		where value = atoi s
atoi s = foldl g 0 s
	 where 
	 g x y = (10 * x) + ((ord y) - 48)

-- atof function ------------------------------------------------------------------
--

atof :: String -> Double
atof s = let (intPart,decimalPart) = break (=='.') s
	 in fromInteger (toInteger (atoi intPart)) + 
		fromInteger (toInteger (atoi (tail decimalPart))) / 
		10.00 ^ fromInteger (toInteger (length decimalPart - 1))

-- doPrim3 function ---------------------------------------------------------------------------
--

doPrim3 :: String -> Expression -> Expression -> Expression -> TypeExp -> [(String,Expression)] -> Int -> Bool -> Bool -> Bool -> (Expression,Bool,Bool,Bool)

doPrim3 funName test e1 e2 t adfs counter rtError halt debug = 
   case funName of
     {
	"if-then-else" ->
	--trace ("doPrim3 if-then-else :"++ show test ++show e1 ++ show e2) $
	case (evalExp test adfs counter rtError halt debug) of
	{ (result,rtError',halt', debug') ->
	  if (not halt') || debug' then
		(errorHandler t, rtError', halt', debug')
	   else
		if result == (Constant "true") then
			evalExp e1 adfs counter rtError' True False
		else if result == (Constant "false") then
			evalExp e2 adfs counter rtError' True False
		else --** a DEBUGGING statement--
  			trace ("if-then-else:  "++ show test ++show result) $
			(errorHandler t, rtError', halt', True)			
	};

	_ ->  --** a DEBUGGING statement--
  	trace ("doPrim3:  "++ show funName ++ show test++show e1++show e2) $
	(errorHandler t, rtError, halt, True)
     }

-- betaReduction funcation ----------------------------------------------------------------------------
-- replace y with x in e
--
betaReduction :: Expression -> String ->  Expression -> Expression

betaReduction y x exp = 
  case exp of
    {
	(Constant v) -> exp;
	(Primitive v) -> exp;
	(Function v) -> exp;
	(List v) -> exp;
	(Variable v) -> if v == x then --trace ("betaReduction: "++show v++show y++show x) $ 
				y 
	       		else Variable v;
	(Application e1 e2 t) -> (Application (betaReduction y x e1)(betaReduction y x e2) t);
	(Lambda a e) ->	(Lambda a (betaReduction y x e))
     }

-- need to deal with free variable, so far we use
-- different lambda-bound variable name to 
-- avoid the problem

-- lookUp function ---------------------------------------------------------------------------
--
-- This function takes 2 arguments: the name of the adf and the list of adfs.
-- It returns the adf (an expression) in the list whose name matches the given
-- name.

lookUp :: String -> [(String, Expression)] -> Expression

lookUp x table = case table of
		{
		 [] -> error "bad reference to ADF";
		 ((y,pt):rest) -> if  (x == y) then pt
				  else lookUp x rest 
		}

--errorHandler function -------------------------------------------------------------------
--
--
errorHandler :: TypeExp -> Expression

errorHandler t = errorHandlerAux t runTimeErrorHandler

errorHandlerAux :: TypeExp -> [(TypeExp, Expression)] -> Expression 

errorHandlerAux aType handlers = 
	case (aType, handlers ) of
	{
	(ListType x, (ListType _, exp):rest) -> exp ;
	(Brackets x, (Brackets _, exp):rest) -> exp ;
	(TypeVar x, (TypeVar _, exp):rest) -> exp ;
	(GenType x, (TypeVar _, exp):rest) -> exp ;
	(aType, (typeExp, exp):rest) ->	if aType == typeExp then exp 
		else errorHandlerAux aType rest ;
	(aType, []) -> trace ("errorHandler : " ++ show aType) $
			--   error "errorHandler"
			(List [])
	}
