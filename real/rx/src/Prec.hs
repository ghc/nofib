module Prec

( glue
)

where

import Maybes

import Syntax
import Ids


import Trace

-------------------------------------------------------------------


glue :: [Either Exp Id] -> Exp

-- handle precedences of operators, input looks like
-- [ Left 3, Right (+), Left 4, Right (*), Left 7, Right (-), Left 5 ]

glue [Left x] = x

-- glue [Left x , Right op , Left y] = App op [x, y]

glue (Left x : rest) = pop (handle rest ([x], []))


pop :: ([Exp],[Id]) -> Exp
-- pop stacks completely
pop ([x],          []      ) = x
pop (x : y : rest, op : ops) = pop (App op [y, x] : rest, ops)


handle :: [Either Exp Id] -> ([Exp],[Id]) -> ([Exp],[Id])

handle [] (args, ops) = (args, ops)
handle inp @ (Right nop : Left arg : rest) (args, ops {- @ ~(op : _) -} ) = 
    let
	 np = the (idprec nop); p = the (idprec op)
	 (op : _) = ops	-- lazily (hbc doesn't like ~ patterns)
    in
	
--	trace ("\nhandle.inp : " ++ show inp) $
--	trace ("\nhandle.args : " ++ show args) $
--	trace ("\nhandle.ops : " ++ show ops) $

    	if not (null ops) && not (exists (idprec nop))
    	then error ("operator has no precedence: " ++ idname nop)

	else if null ops || np > p	-- push it
    	then handle rest (arg : args, nop : ops)

	else if np < p	-- pop it
    	then handle inp  ( App op [args !! 1, args !! 0] : drop 2 args
			   , tail ops)


	-- here, precedence levels coincide
	-- therefore operators must be identical
	else if nop /= op 
	then error ("same precedences: " ++ idname nop ++ ", " ++ idname op)

	-- now they are the same
	else case idbind op of
	    Nn -> error ("not associative at all: " ++ idname op)
	    Rght -> -- push it
		handle rest (arg : args, nop : ops)
	    Lft -> -- pop it
		handle inp ( App op [args !! 1, args !! 0] : drop 2 args
			   , tail ops)


handle inp (args, ops) = 
    error ("strange case for handle: "
	++ "\nhandle.inp : " ++ show inp
	++ "\nhandle.args : " ++ show args
	++ "\nhandle.ops : " ++ show ops)
