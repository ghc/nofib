{---------------------------------------------------------------
 --
 -- Create.hs : contains function defination to generate program
 --		parse trees.
 -- T.Yu@cs.ucl.ac.uk	September 25, 1997
 --
 --------------------------------------------------------------}

module Create (createTree,extract)where
import Header (TypeExp(..), Expression(..),ParseTree(..))
import Local (termEnv,funEnv,constant,adfs,args) 
import Unify (applySub, unify, Theta(..))
import Array

-- selectTerm function -----------------------------------------------------------------
--
-- This function takes a type expression, a theta, a randomList and typeNum. It return a tuple
-- of the following elements: flag indicates whether a terminal is selected, the name
-- of the terminal, theta created, new random list and new typeNum.
-- If the expected retrun type contains temporary tyep variables, 
-- we first instaniate dummy type variables in the selected terminal type
-- with new temporary type variables before "unifying" it with the return 
-- type.

selectTerm :: TypeExp -> Theta -> [Int] -> Int -> (Bool, String,Theta, [Int], Int)

selectTerm retType theta rList typeNum = 
  let (start, end) = bounds termEnv
      index = (head rList `mod` end) + start
      match currIndex init = 
	if not init && (currIndex == index) then
		(False, "", theta, tail rList, typeNum)
	else if currIndex > end then
		match start init
	else 
		case ( termEnv ! currIndex ) of
		{
		(name,typeSig) ->
		   if hasTypeVar retType then
			case (instDummy typeSig typeNum []) of
			{
			(typeSig', typeNum', dummyTheta) ->
			   case unify True [(retType, typeSig')] theta of
			   {
			   (unifiable, theta') ->
			      if unifiable then
				(True, name, theta', tail rList, typeNum')
			      else
				match (currIndex +1) False
			    }
			}
		   else
		      case ( unify True [(retType,typeSig)][] ) of
		      {
		        (unifiable, theta') -> 
		      	  if unifiable then
				(True, name, theta, tail rList, typeNum)
			  else
				match (currIndex +1) False
			}
		}
  in
	    match index True

-- selectFun function -----------------------------------------------------------------
--
-- This function takes a type expression, a theta, first and last index and typeNum. 
-- It return a tuple of the following elements: flag indicates whether a function is selected, 
-- the name of the function, its argument type signatuer, theta created, index of the function
-- and new typeNum.
-- If the expected retrun type contains temporary tyep variables, 
-- we first instaniate dummy type variables in the selected function type
-- with new temporary type variables before "unifying" it with the return 
-- type.

selectFun :: TypeExp -> Theta -> Int -> Int -> Int -> (Bool, String, TypeExp, Theta, Int, Int)

selectFun retType theta first last typeNum =
  --trace("selectFun : " ++ show first ++ show last ) $
  let (start, end) = bounds funEnv
      match currIndex init = 
	if not init && (currIndex == last) then
		(False, "", retType, theta, first, typeNum) -- retType means nothing
	else if currIndex > end then
		match start init
	else
		let (name,typeSig) = funEnv ! currIndex
		in if hasTypeVar retType then
			let (typeSig', typeNum', dummyTheta) = instDummy typeSig typeNum []
			    (unifiable, argsType,theta') = getArgsType retType typeSig' theta
			in
			  if unifiable then
				(True, name, argsType, theta', currIndex, typeNum')
			  else
				match (currIndex +1) False
		   else
			let (unifiable, argsType,theta') = getArgsType retType typeSig []
			in if unifiable then
				let (argsType', typeNum', dummyTheta) = instDummy argsType typeNum []
				in (True, name, argsType', theta, currIndex, typeNum')
			   else
				match (currIndex + 1) False
  in
    match first True


-- instDummy function -----------------------------------------------------------------------
--
-- This fucntion takes a type expression and instantiates all dummy type variables with
-- temporary type variables. It returns the new type expression and new TypeNum

instDummy :: TypeExp -> Int -> Theta -> (TypeExp,Int,Theta)

instDummy typeExp typeNum theta =
	case typeExp of
	{
	(DummyType x) -> let typeExp' = TypeVar ("T"++show typeNum)
			 in (typeExp',(typeNum + 1),(x,typeExp'):theta);
	(Arrow t1 t2) -> let (t1', typeNum', theta') = instDummy t1 typeNum theta
			     (t2', typeNum'', theta'') = instDummy (applySub theta' t2) typeNum' theta'
			 in (Arrow t1' t2', typeNum'', theta'');
	(Brackets t) -> let (t', typeNum', theta') = instDummy t typeNum theta
			in (Brackets t',typeNum', theta');
	(ListType t) -> let (t', typeNum', theta') = instDummy t typeNum theta
			in (ListType t',typeNum', theta');
	_ -> (typeExp, typeNum, theta)
	}

-- hasTypeVar function ---------------------------------------------------------
--
--

hasTypeVar :: TypeExp -> Bool

hasTypeVar typeExp = 
	case typeExp of
	{
	(TypeVar _) -> True;
	(Arrow t1 t2) -> (hasTypeVar t1) || (hasTypeVar t2);
	(Brackets t) -> hasTypeVar t;
	(ListType t) -> hasTypeVar t;
	_ -> False
	}

-- createTree function ----------------------------------------------------------
--
-- This function takes 6 arguments: a depth level, return type, randomList, theta typeNum and genTypes.
-- It returns a ParseTree with the specifed depth and return type.
-- We basically use "full" method unless no non-terminal to match the required type.
-- In that case, we pick a terminal and stop growing. 

createTree ::  Int -> TypeExp -> [Int] -> Theta -> Int -> ( ParseTree, [Int], Theta, Int)

createTree 1 retType rList theta typeNum =
 --trace ("create1 "++show retType++show theta) $
  let retType' = applySub theta retType
      (findOne, name, theta', rList', typeNum') = selectTerm retType' theta rList typeNum
  in 
   --trace ("selectTerm: "++show name++show theta') $	
   if not findOne then    -- fail, no variable that matches the return type
	(Empty, rList, theta, typeNum)
   else	
	if (elem name constant) then
	   (ExpCons (Constant name), rList', theta', typeNum')
   else
	if name == "nil" then
	   (ExpCons (List []), rList', theta', typeNum')
	else
	   (ExpCons (Variable name), rList', theta', typeNum')

createTree level retType rList theta typeNum =
 --trace ("create "++show level++show retType++show theta) $
    let retType' = applySub theta retType
	(start, end) = bounds funEnv
	orgIndex = (head rList `mod` end) + start	
    in
	--trace ("info: "++show retType'++show start++show end++show orgIndex) $	
	let f1 first last init rList = 
		if not init && (first == last) then
			createTree 1 retType' rList theta typeNum -- create leaf ( grow method )
		else
		let (findOne, name, argsType, theta', index, typeNum')=
			selectFun retType' theta first last typeNum
		in 
		  --trace ("selectFun: "++show findOne++show name++show argsType++show theta'++show index) $	
		  if not findOne then 	  -- fail, no function matches the return type,
			createTree 1 retType' rList theta typeNum -- create leaf ( grow method )
		  else
			let f2 argType retType rList theta typeNum =
				case argType of
				{
				(Arrow t1 t2) -> 
				let getRetType t = 
					case t of
					{
					(Arrow t1 s@(Arrow t2 t3)) -> 
						let (aType, rType)= getRetType s
						in  (Arrow t1 aType, rType);
					(Arrow t1 t2) -> (t1,t2)
					}
				    (argType', newRetType) = getRetType argType
				    (exp2, rList', theta', typeNum') =
						createTree (level-1) newRetType rList theta typeNum
				in if exp2 == Empty then 
					(Empty,Empty,rList',[],typeNum')
				   else 
					let argType'' = applySub theta' argType'
					    newRetType' = applySub theta' newRetType
					    (exp1', exp2',rList'',theta'',typeNum'') = 
					        f2 argType'' (Arrow newRetType' retType) rList' theta' typeNum'
					in if (exp1'==Empty) || (exp2'==Empty) then
						(Empty,Empty,rList'',[],typeNum'')
					     else
						(ExpCons (Application (extract exp1') (extract exp2') 
							(Arrow (applySub theta'' newRetType')
						(applySub theta'' retType))), exp2, rList'', theta'', typeNum'');
				_ -> case (createTree (level-1) argType rList theta typeNum) of
			     		{ 
					(exp2,rList',theta',typeNum') ->
				      		if exp2 == Empty then 
					   	(Empty, Empty, rList, [], typeNum)
				      		else
					   	if (elem name adfs) then
							( ExpCons(Function name), exp2, rList', theta', typeNum')
					    	else
							if (elem name args ) then
						 	( ExpCons (Variable name), exp2, rList', theta', typeNum')
					      		else
						 	( ExpCons(Primitive name), exp2, rList', theta', typeNum')
			     		}
				}
			in
			--trace ("f1 in "++show name++show argTypes++show retType'++show newTheta) $
			case (f2 argsType retType' rList theta' typeNum') of
			{ 
			(exp1, exp2, rList', theta'', typeNum'') ->
			if (exp1==Empty) || (exp2==Empty) then
				f1 (index +1) last False rList' 
			else
				(ExpCons (Application (extract exp1)(extract exp2) retType'), rList', theta'', typeNum'')
			}
	in	
	  f1 orgIndex orgIndex True (tail rList)




-- getArgsType function ----------------------------------------------------------------
--
-- This function takes an expected type and a function type. It unify the expected type
-- with the function return type. It then instaniate the argument type using the theta.
-- It returns the instantiated argument type.

getArgsType :: TypeExp -> TypeExp -> Theta -> (Bool, TypeExp, Theta)

getArgsType retType typeExp theta =
	let unifyRetType aType theta = case aType of
			{
			 (Arrow argType rType) -> unifyRetType rType theta; 
			 _ -> unify True [(retType,aType)] theta
			}
	    (unifiable,theta') = unifyRetType typeExp theta
	in if unifiable then
	   let  typeExp' = applySub theta' typeExp
	    	retType' = applySub theta' retType	    
	    	f exp = case exp of
		   {
		    (Arrow t1 t2) -> if t2 == retType' then t1
					else (Arrow t1 (f t2));
	  	    _ -> error ("error in getArgsType ")
		    }
	   in
	    	(True, (f typeExp'), theta')
	   else
		(False, typeExp, theta)	    

--no need to deal with arrow situation since functions are curried

--extract -----------------------------------------------------------------------------
extract :: ParseTree -> Expression

extract exp = case exp of
	{
	Empty -> error "Empty expression";
	(ExpCons x) -> x
	}

