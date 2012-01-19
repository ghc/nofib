{---------------------------------------------------------------
 --
 -- Header.hs : contains type definition for the system.
 -- T.Yu@cs.ucl.ac.uk	September 25, 1997
 --
 --------------------------------------------------------------}

module Header where

-- TypeExp data type --------------------------------------------------------------------------
--
-- This is the type subexpression language used in the system.

data TypeExp =  IntNum |
		Boolean |
		Str    |
		ListType TypeExp  | 
		Arrow TypeExp TypeExp |
--		TypeExp :--> TypeExp
		Brackets TypeExp |
		TypeVar String |
		DummyType String |
		GenType String 
		deriving (Eq,Show)


		      
-- ParseTree data type -----------------------------------------------------------------
-- 
-- Pares Tree is either an Empty tree or an Expression

data ParseTree = Empty 
		| ExpCons Expression
		  deriving (Eq,Show)	

-- Expression data type ------------------------------------------------------------
--
--

type ExpressionList = [Expression]
type Population = [(Expression, Double)] 
--type Population = [(Expression, Temp ,Double)] --for testing eval function
--type Temp = (Expression,Bool,Bool)

-- data Program = Main Expression [(ADF,String)]	-- ADFs and their names

-- data Program = Start Expression
-- data ADF     = Lambda1 String Expression TypeExp
--              | LambdaN String ADF TypeExp

data Expression = Constant String 
		 | List ExpressionList -- (List []) is the empty list
    	         | Variable String
		 | Primitive String
                 | Function String	-- name of ADF - see list above
                 | Application Expression Expression TypeExp 
		 | Lambda String Expression
		   deriving (Eq,Show)	


