
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.Dfa
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005
-- License     :  LGPL
--
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Parsing (through parsing combinators) concrete regular Expressions
--                (in the Unix like notation)
--
-- Code Included in the Lecture Notes on
--      Language Processing (with a functional flavour).
--
-----------------------------------------------------------------------------


module Language.HaLex.RegExpParser ( parseRegExp
                                   ) where

import Data.Char
import Language.HaLex.Parser
import Language.HaLex.RegExp


-- | Parser for regular expressions

parseRegExp :: [Char]                -- ^ Input symbols
            -> Maybe (RegExp Char)   -- ^ Regular expression
parseRegExp re = res
  where parsed_re = expr re
        res | parsed_re == [] = Nothing
            | otherwise       = Just (fst (head parsed_re))

expr :: Parser Char (RegExp Char)
expr =  f  <$> termThen <*> symbol '|' <*> expr
    <|> id <$> termThen
    <|> succeed Epsilon
  where f l _ r = Or l r

termThen :: Parser Char (RegExp Char)
termThen =  f  <$> term <*> termThen
        <|> id <$> term
  where f l r   = Then l r

term :: Parser Char (RegExp Char)
term =  f  <$> factor <*> symbol '?'
    <|> g  <$> factor <*> symbol '*'
    <|> h  <$> factor <*> symbol '+'
    <|> id <$> factor
  where
     f e _ = Or   e Epsilon
     g e _ = Star e
     h e _ = Then e (Star e)

factor :: Parser Char (RegExp  Char)
factor =  f <$> letterOrDigit
      <|> g <$> symbol '\'' <*> satisfy (\ x -> True) <*> symbol '\''
      <|> h <$> symbol '(' <*> expr <*> symbol ')'
  where
     f a     = Literal a
     g _ e _ = Literal e
     h _ e _ = e

letterOrDigit :: Parser Char Char
letterOrDigit = satisfy (\x -> isDigit x || isAlpha x)

-- Not used yet
spaces :: Parser Char ()
spaces = (\ _ _ -> ()) <$> symbol ' ' <*> spaces
      <|> succeed ()
