{-----------------------------------------------------------------
 
  (c) 2009-2010 Markus Dittrich 
 
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

-- | module parsing an infix math equation into an compute stack
-- in reverse polish notation (rpn)
module RpnParser ( parse_infix_to_rpn ) where

-- imports
import Control.Monad
import Prelude

-- local imports
import ExtraFunctions
import GenericModel
import RpnData
import TokenParser

-- import Debug.Trace

-- | parses a mathematical infix expression and converts
-- into a stack in rpn
parse_infix_to_rpn :: CharParser ModelState RpnStack 
parse_infix_to_rpn = RpnStack <$> add_term
                  <?> "infix math expression"



-- | parser for expressions chained via "+" 
add_term :: CharParser ModelState [RpnItem]
add_term = concat . insert_adds <$> 
             sub_term `sepBy` (reservedOp "+")
        <?> "addition term"
  
  where
    insert_adds [] = []
    insert_adds (x:xs) = x:foldr (\y a -> y:[BinFunc (+)]:a) [] xs



-- | parser for expressions chained via "-"
sub_term :: CharParser ModelState [RpnItem]
sub_term = concat . insert_subs <$> 
             div_term `sepBy` (reservedOp "-")
        <?> "subtraction term"
  
  where
    insert_subs [] = []
    insert_subs (x:xs) = x:foldr (\y a -> y:[BinFunc (-)]:a) [] xs



-- | parser for expressions chained via "*" 
div_term :: CharParser ModelState [RpnItem]
div_term = concat . insert_divs <$> 
             mul_term `sepBy` (reservedOp "/")
        <?> "division term"

  where
    insert_divs [] = []
    insert_divs (x:xs) = x:foldr (\y a -> y:[BinFunc (/)]:a) [] xs



-- | parser for expressions chained via "/"
mul_term :: CharParser ModelState [RpnItem]
mul_term = concat . insert_muls <$> 
             exp_term `sepBy` (reservedOp "*")
        <?> "product term"

  where
    insert_muls [] = []
    insert_muls (x:xs) = x:foldr (\y a -> y:[BinFunc (*)]:a) [] xs



-- | parser for potentiation operations "^"
exp_term :: CharParser ModelState [RpnItem]
exp_term = concat . insert_exps <$> factor `sepBy` (reservedOp "^")
        <?> "exponent"

  where
    insert_exps [] = []
    insert_exps (x:xs) = x:foldr (\y a -> y:[BinFunc real_exp]:a) [] xs



-- | parser for individual factors, i.e, numbers,
-- variables or operations
factor :: CharParser ModelState [RpnItem]
factor = try parse_single_number  -- need try due to the unary "-"
      <|> try signed_parenthesis  -- (otherwise we get stuck)
      <|> parse_functions
      <|> parse_variable
      <?> "token or variable"         



-- | parse all operations of type (Double -> Double)
-- we currently know about
parse_functions :: CharParser ModelState [RpnItem]
parse_functions = (msum $ extract_ops builtinFunctions)
               <?> "builtin unary function"
  where
    extract_ops = foldr (\(x,y) acc -> 
                         ((reserved x *> execute y):acc)) [] 
    execute op  =  ( (insert_func op) <$>
                    (  parens add_term 
                    <|> parse_single_number
                    <|> parse_variable
                    ))
               <?> "function parsing"

    insert_func _ [] = []
    insert_func f xs = xs ++ [UnaFunc f]



-- | parse a potentially signed expression enclosed in parenthesis.
-- In the case of parenthesised expressions we 
-- parse -(...) as (-1.0)*(...)
signed_parenthesis :: CharParser ModelState [RpnItem]
signed_parenthesis = push_parens <$> parse_sign <*> parens add_term
                  <?> "signed parenthesis"

  where
    push_parens sign xs = xs ++ [Number sign,BinFunc (*)]



-- | parse a single number; integers are automatically promoted 
-- to double
-- NOTE: Due to the notFollowedBy this parser can not be used
-- with 'many' and other parser combinators.
parse_single_number :: CharParser ModelState [RpnItem]
parse_single_number = push <$> (parse_number) 
                   <?> "signed integer or double"
  where
    push x = [Number x]



-- | parse a number, can be used with 'many' and other parser
-- combinators; integers are automatically promoted to double
parse_number :: CharParser ModelState Double
parse_number = converter <$> parse_sign <*> naturalOrFloat 
            <?> "signed integer or double"
  where 
    converter sign val = case val of
                           Left i  -> sign * (fromInteger i)
                           Right x -> sign * x



-- | parse the sign of a numerical expression
parse_sign :: CharParser ModelState Double
parse_sign = option 1.0 ( char '-' *> pure (-1.0) )
          <?> "sign"



-- | this is how valid variable names have to look like
parse_variable :: CharParser ModelState [RpnItem]
parse_variable = push <$> parse_sign <*> identifier 
              <?> "variable"
  where
    -- in case of a unary minus we also push the necessary
    -- multiplication by (-1) onto the stack
    push sign x =
      if sign >= 0 
        then
          if x == "TIME" 
            then [Time]
            else [Variable x]
        else
          if x == "TIME"
            then [Time,Number (-1.0),BinFunc (*)]
            else [Variable x, Number (-1.0), BinFunc(*)]

