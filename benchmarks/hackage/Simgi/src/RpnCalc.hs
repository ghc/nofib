{-----------------------------------------------------------------
 
  (c) 2009-2010 Markus Dittrich,
      National Resource for Biomedical Supercomputing &
      Carnegie Mellon University
 
 
  This program is free software; you can redistribute it 
  and/or modify it under the terms of the GNU General Public 
  License Version 3 as published by the Free Software Foundation. 
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License Version 3 for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330, 
  Boston, MA 02111-1307, USA.

--------------------------------------------------------------------}

-- | RpnCalc defines the data structures and a calculator engine
-- for computing mathematical expressions that have been parsed
-- into reverse polish notations
module RpnCalc ( get_val_from_symbolTable 
               , rpn_compute
               , try_evaluate_expression
               ) where


-- imports 
import qualified Data.Map as M
import Prelude

-- local imports
import GenericModel
import RpnData


-- | computes an expressions based on an rpn stack
-- molecule names are looked up in a MoleculeMap
-- NOTE: This function expects the RPNstack to be sanitized
-- with respect to the variables, i.e., all variables in
-- the stack are assumed to exist in the VariableMap
rpn_compute :: SymbolTable -> Double -> RpnStack -> Double
rpn_compute _      _   (RpnStack [(Number x)]) = x
rpn_compute symbols theTime (RpnStack xs)       = num 

  where
    (Number num) = head . foldl evaluate [] $ xs

    -- evaluate unary function (sin, cos, ..)
    evaluate ((Number x):ys) (UnaFunc f) = 
      (Number $ f x):ys

    -- evaluate binary function (*,+,..)
    evaluate ((Number x):(Number y):ys) (BinFunc f) =
      (Number $ f y x):ys

    -- extract current time
    evaluate ys (Time) = (Number theTime):ys

    -- extract molecule variable
    evaluate ys (Variable x) = 
      (Number $ get_val_from_symbolTable x theTime symbols):ys 

    evaluate ys item = item:ys


-- | retrieve the value of a given symbol (either variable or molecule count) from
-- the symbol table 
get_val_from_symbolTable :: String -> Double -> SymbolTable -> Double
get_val_from_symbolTable var aTime symbols =
  
  case M.lookup var (molSymbols symbols) of
    Just value -> fromIntegral value
    Nothing    -> case (M.!) (varSymbols symbols) var of
                    Constant c -> c
                    Function s -> rpn_compute symbols aTime s



-- | try to evaluate an RPN stack and return the result as a Double
-- if we can evaluate it during parse-time (i.e. if it doesn't contain
-- things like time and molecule count)
-- NOTE: We do _not_ want to substitute any molecule counts even
-- if we should now them; molecule counts are inherently dynamic
-- and need to be evaluated at run-time
try_evaluate_expression :: RpnStack -> VariableMap -> Either RpnStack Double
try_evaluate_expression stack varMap = 

  if can_evaluate stack 
    then
      Right $ rpn_compute (SymbolTable M.empty varMap) 0.0 stack 
    else
      Left stack

  where

    can_evaluate = null . filter unknown_element . toList 
      where
        unknown_element x = case x of
                              Time       -> True
                              Variable v -> if v `elem` M.keys varMap
                                            then False
                                            else True
                              _          -> False
                                                   
