-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HaLex.RegExp
-- Copyright   :  (c) João Saraiva 2001,2002,2003,2004,2005
-- License     :  LGPL
-- 
-- Maintainer  :  jas@di.uminho.pt
-- Stability   :  provisional
-- Portability :  portable
--
-- Regular Expressions in Haskell. 
-- 
-- Code Included in the Lecture Notes on 
--          Language Processing (with a functional flavour).   
--
-----------------------------------------------------------------------------

module Language.HaLex.RegExp ( 
              -- * Data type with recursion pattern
                RegExp (..)
              , cataRegExp
              -- * Matching
              , matchesRE
              , matches'
              -- * Size
              , sizeRegExp
              -- * Printing
              , showRE
              -- * Simplification
              , simplifyRegExp
              -- * Normalization
              , extREtoRE
              ) where

-----------------------------------------------------------------------------
-- * Data type with recursion pattern

-- | Type of regular expressions.
data RegExp sy  = Empty                              -- ^ Empty Language
                | Epsilon                            -- ^ Empty String
                | Literal   sy                       -- ^ Literals
                | Or        (RegExp sy) (RegExp sy)  -- ^ Disjuncion
                | Then      (RegExp sy) (RegExp sy)  -- ^ Sequence
                | Star      (RegExp sy)              -- ^ Repetition, possibly zero time
                | OneOrMore (RegExp sy)              -- ^ One or more times (extended RegExp)
                | Optional  (RegExp sy)              -- ^ Optional (extended RegExp)
   deriving (Read, Eq)
   
-- | Catamorphism induced by the 'RegExp' inductive data type

cataRegExp :: ( re 
              , re
              , re -> re -> re
              , re -> re
              , sy -> re
              , re -> re -> re
              , re -> re
              , re -> re
              ) -> RegExp sy -> re

cataRegExp (empty,epsilon,or,star,lit,th,one,opt) = cata
 where cata Empty          = empty 
       cata Epsilon        = epsilon
       cata (Or er1 er2)   = or (cata er1) (cata er2)
       cata (Star er)      = star (cata er)
       cata (Literal a)    = lit a
       cata (Then er1 er2) = th (cata er1) (cata er2)
       cata (OneOrMore er) = one (cata er)
       cata (Optional er)  = opt (cata er)

-----------------------------------------------------------------------------
-- * Matching

-- | Test whether a match can be found for the given regular expression
--   in the given sequence of characters. The regular expression is
--   assumed not to contain 'OneOrMore' or 'Optional'. See also @matches'@.

matchesRE :: Eq sy 
          => RegExp sy         -- ^ (canonical) Regular Expression
          -> [sy]              -- ^ Input Symbols
          -> Bool
matchesRE Empty inp          = False
matchesRE Epsilon inp        = inp == []
matchesRE (Literal l) inp    = ([l] == inp)
matchesRE (Or re1 re2) inp   = matchesRE re1 inp || matchesRE re2 inp 
matchesRE (Then re1 re2) inp = or [ matchesRE re1 s1 && matchesRE re2 s2 
                                  | (s1,s2) <- splits inp]
matchesRE (Star re) inp      = matchesRE Epsilon inp ||
                               or [ matchesRE re s1 && matchesRE (Star re) s2 
                                  | (s1,s2) <- frontSplits inp ]

-- | Test whether a match can be found for the given regular expression
--   in the given sequence of characters. The regular expression is
--   allowed to contain 'OneOrMore' or 'Optional'.

matches' :: Eq sy 
         => RegExp sy          -- ^ Regular Expression 
         -> [sy]               -- ^ Input Symbols
         -> Bool
matches' = matchesRE . extREtoRE

-- | Produce a list of all possible ways of splitting the input list
--   into two parts. For instance, 
-- @ 
--   splits "foo" 
--     = [(\"\","foo"),("f","oo"),("fo","o"),("foo",\"\")] 
-- @

splits :: [a]                -- ^ Input List
       -> [ ([a],[a]) ]      -- ^ Splited List
splits s = [ splitAt n s | n <- [ 0 .. length s ] ]


-- | Produce a list of all possible ways of splitting the input list
--   into two parts where the first part is non-empy. For instance, 
-- @ 
--   splits "foo" 
--     = [("f","oo"),("fo","o"),("foo",\"\")]
-- @

frontSplits :: [a] -> [ ([a],[a]) ]
frontSplits s = [ splitAt n s | n <- [ 1 .. length s ] ]


-----------------------------------------------------------------------------
-- * Size

-- | Compute the size of a regular expression.
--   We define the size of a regular expression as the number of occurrences 
--   of symbols of the alfabeth

sizeRegExp :: RegExp sy      -- ^ Regular Expression
           -> Int            -- ^ Size
sizeRegExp = cataRegExp (0,0,(+),id,\x -> 1,(+),id,id)


-----------------------------------------------------------------------------
-- * Printing

-- | Print regular expression to String as a catamorphism.
--   A straightforward (catamorphic) show function.
--
--   (it produces too many brackets, making it difficult to read or 
--    understand the expression)

showRE :: Show sy 
       => RegExp sy        -- ^ Regular Expression
       -> [Char]           -- ^ String-based Regular Expression
showRE = cataRegExp  ("{}"
                     , "@"
                     , \ l r -> "(" ++ l ++ "|" ++ r ++ ")"
                     , \ er  -> "(" ++ er ++ ")*"
                     , show 
                     , \ l r -> "(" ++ l ++ r ++ ")"
                     , \ er  -> "(" ++ er ++ ")+"
                     , \ er  -> "(" ++ er ++ ")?"
                     )

-- | Pretty print of regular expressions.

instance Show sy => Show (RegExp sy) where
          showsPrec _ Empty             = showString "{}"
 	  showsPrec _ Epsilon           = showChar '@'
 	  showsPrec _ (Literal c)       = showsPrec 0 c
{-                | isSymbol c            = showChar '\''
                                        . showChar c
                                        . showChar '\''
                | otherwise             = showChar c
-}
   	  showsPrec n (Star e)          = showsPrec 10 e . showChar '*'
 	  showsPrec n (OneOrMore e)     = showParen (n == 4)
                                        $ showsPrec 10 e
                                        . showChar '+'
 	  showsPrec _ (Optional e)      = showsPrec 10 e
                                        . showChar '?'
 	  showsPrec n (e1 `Or` e2)      = showParen (n /= 0 && n /= 4)
                                        $ showsPrec 4 e1
                                        . showChar '|'
                                        . showsPrec 4 e2
	  showsPrec n (e1 `Then` e2)    = showParen (n /= 0 && n /= 6)
                                        $ showsPrec 6 e1
                                        . showChar ' '
                                        . showsPrec 6 e2
 
isSymbol x = x `elem` "|? "

-----------------------------------------------------------------------------
-- * Simplification

-- | Simplify regular expressions according to the algebra of regular expressions.

simplifyRegExp :: Eq sy => RegExp sy -> RegExp sy
simplifyRegExp Empty        = Empty
simplifyRegExp Epsilon      = Epsilon
simplifyRegExp (Literal x)  = Literal x

simplifyRegExp (Star x)     = case x' of                                -- Algebraic Rules:
                               Epsilon      -> Epsilon                  -- @*       = @
  		               Empty        -> Epsilon                  -- {}*      = @
                               Or Epsilon a -> Star (simplifyRegExp a)  -- (a | @)* = a*
                               Or a Epsilon -> Star (simplifyRegExp a)  -- (@ | a)* = a*
                               _            -> Star x'
  where x' = simplifyRegExp x 


simplifyRegExp (Then x y)  | x' == Empty    = Empty                     -- {} p = {}
                           | y' == Empty    = Empty                     -- p {} = {}
                           | x' == Epsilon  = y'                        -- @ p  = p
                           | y' == Epsilon  = x'                        -- p @  = p
                           | y' == Star x'  = OneOrMore x'              -- p p* = p+
                           | x' == Star y'  = OneOrMore y'              -- p* p = p+
                           | otherwise      = Then x' y'
  where x' =  simplifyRegExp x
        y' =  simplifyRegExp y


simplifyRegExp a@(Or x y)  
      | x' == y'                       = x'                             -- p  | p  = p
      | x' == Empty                    = y'                             -- {} | p  = p 
      | y' == Empty                    = x'                             -- p  | {} = p
--    | x == (Star a) && y == Epsilon  = simplifyRegExp x       
      | otherwise                      = f x' y'                        -- Or x' y'
  where x' = simplifyRegExp x 
        y' = simplifyRegExp y

        f Epsilon (OneOrMore p) = Star p                                -- (@ | p+) = p*
        f Epsilon re            = Optional re                           -- p | @    = p? 
        f (OneOrMore p) Epsilon = Star p                                -- (p+ | @) = p*
        f re Epsilon            = Optional re                           -- @ | p    = p? 
        f re1 re2               = Or re1 re2


simplifyRegExp (OneOrMore x) = case x' of
                                Empty        -> Empty                   -- {}+      = {}
                                Epsilon      -> Epsilon                 -- @+       = @
                                Or p Epsilon -> Star p                  -- (p | @)+ = p*
                                Or Epsilon p -> Star p                  -- (@ | p)+ = p*
                                _            -> OneOrMore x'
  where  x' = simplifyRegExp x

simplifyRegExp (Optional x) = Optional (simplifyRegExp x)


-----------------------------------------------------------------------------
-- * Normalization

-- | Rewrite extended regular expressions to
--   plain regular expression. This means that the 'OneOrMore' 
--   and 'Optional' constructors are normalized away.

extREtoRE :: RegExp sy -> RegExp sy
extREtoRE  = cataRegExp ( Empty
                        , Epsilon 
                        , \ l r -> Or l r
                        , \ er  -> Star er
                        , \ a   -> Literal a 
                        , \ l r -> Then l r 
                        , \ er  -> Then er (Star er)
                        , \ er  -> Or Epsilon er
                        ) 

-----------------------------------------------------------------------------


