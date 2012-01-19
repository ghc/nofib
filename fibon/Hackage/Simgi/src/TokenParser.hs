{-----------------------------------------------------------------
 
  (c) 2008-2009 Markus Dittrich 
 
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

-- | functionality related to parsing tokens
module TokenParser ( module Control.Applicative
                   , module Text.ParserCombinators.Parsec
                   , braces
                   , brackets
                   , builtinFunctions
                   , colon
                   , comma
                   , commaSep
                   , charLiteral
                   , float
                   , identifier
                   , integer
                   , lineToken
                   , parens
                   , keywords
                   , lexer
                   , naturalOrFloat
                   , operator
                   , operators
                   , reservedOp
                   , reserved
                   , semi
                   , stringLiteral
                   , symbol
                   , whiteSpace
                   ) where


-- imports
import Control.Applicative
import Control.Monad (ap, MonadPlus (..))
import Prelude
import Text.ParserCombinators.Parsec hiding (many,optional, (<|>)) 
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language (haskellDef
                                              , reservedOpNames
                                              , reservedNames )

-- local imports
import ExtraFunctions


{- Definitions for Applicative Parsec instance -}

-- | Applicative instance for Monad
instance Applicative (GenParser s a) where
  pure  = return
  (<*>) = ap



-- |Alternative instance for MonadPlus
instance Alternative (GenParser s a) where
  empty = mzero
  (<|>) = mplus



{- set up the Token Parser -}

-- | builtin functions of the form (Double -> Double)
builtinFunctions :: [(String, Double -> Double)]
builtinFunctions = [ ("sqrt",sqrt)
                   , ("exp",exp)
                   , ("log",log)
                   , ("log2", logBase 2)
                   , ("log10", logBase 10)
                   , ("sin",sin)
                   , ("cos",cos)
                   , ("tan",tan)
                   , ("asin", asin)
                   , ("acos", acos)
                   , ("atan", atan)
                   , ("sinh", sinh)
                   , ("cosh", cosh)
                   , ("tanh", tanh)
                   , ("asinh", sinh)
                   , ("acosh", cosh)
                   , ("atanh", atanh)
                   , ("erf", erf)
                   , ("erfc", erfc)
                   , ("abs", abs)
                   ]



-- | all other keywords that are not regular functions
keywords :: [String]
keywords =  [ "end", "events", "molecules", "output", "outputBuffer"
            , "outputFile", "outputFreq", "parameters", "molecules", "reactions"
            , "systemVol", "time", "variables"] 

operators :: [String]
operators = ["+", "->", "::", "=", "{", "}", ">=", "==", "<="
            , "<", ">", "*", "/", "-", "&&"]



-- | function generating a token parser based on a 
-- lexical parser combined with a language record definition
lexer :: PT.TokenParser st
lexer  = PT.makeTokenParser 
         ( haskellDef { reservedOpNames = operators
                      , reservedNames = keywords 
                            ++ map fst builtinFunctions
                      } )



-- | parser for parser sandwitched between line
-- symbols '|'
lineToken :: CharParser st a -> CharParser st a
lineToken = between (symbol "|") (symbol "|") 


-- | token parser for parenthesis
parens :: CharParser st a -> CharParser st a
parens = PT.parens lexer



-- | token parser for parenthesis
braces :: CharParser st a -> CharParser st a
braces = PT.braces lexer


-- | token parser for brackets 
brackets :: CharParser st a -> CharParser st a
brackets = PT.brackets lexer


-- | token parser for Integer
integer :: CharParser st Integer
integer = PT.integer lexer


-- | token parser for Char
stringLiteral :: CharParser st String
stringLiteral = PT.stringLiteral lexer


-- | token parser for Char
charLiteral :: CharParser st Char
charLiteral = PT.charLiteral lexer


-- | token parser for Double
float :: CharParser st Double
float = PT.float lexer


-- | token parser for Either Integer Double
naturalOrFloat :: CharParser st (Either Integer Double)
naturalOrFloat = PT.naturalOrFloat lexer


-- | token parser for keywords
reservedOp :: String -> CharParser st ()
reservedOp = PT.reservedOp lexer



-- | token parser for keywords
reserved :: String -> CharParser st ()
reserved = PT.reserved lexer



-- | token parser for whitespace
whiteSpace :: CharParser st ()
whiteSpace = PT.whiteSpace lexer



-- | token parser for colon
colon:: CharParser st String
colon = PT.colon lexer



-- | token parser for semicolon
semi :: CharParser st String
semi = PT.semi lexer



-- | token parser for comma
comma :: CharParser st String
comma = PT.comma lexer


-- | token parser for comma separated list of items
commaSep :: CharParser st a -> CharParser st [a]
commaSep = PT.commaSep lexer


-- | token parser for symbol
symbol :: String -> CharParser st String
symbol = PT.symbol lexer


-- | token parser for symbol
identifier :: CharParser st String
identifier = PT.identifier lexer


-- | token parser for symbol
operator:: CharParser st String
operator = PT.operator lexer


