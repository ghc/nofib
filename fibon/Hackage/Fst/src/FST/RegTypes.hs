{-
   **************************************************************
   * Filename      : RegTypes.hs                                *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 5 July, 2001                               *
   * Lines         : 219                                        *
   **************************************************************
-}

module FST.RegTypes ( Reg(..),      -- data type for the regular expression
                  Combinators,  -- Type class for Combinators.
		  (<|>),        -- Union combinator
		  (|>),         -- Concatenation combinator
		  (<&>),        -- Intersection combinator
		  (<->),        -- Minus combinator
		  s,            -- Symbol
		  eps,          -- Epsilon
		  empty,        -- Empty
		  complement,   -- Complement
		  star,         -- Star
		  plus,         -- Plus
		  allS,         -- All Symbol
		  allToSymbols, -- transform the 'all' symbol to union over
		                -- alphabet.
		  allFree,      -- free a regular expression from 'all'
		                -- symbols.
		  reversal,     -- reverse a regular expression.
		  acceptEps,    -- Does the regular expression accept epsilon?
		  Symbols,      -- Type class for Symbols.
		  symbols       -- Collect the symbols in a
		                -- regular expression.
	        ) where

import Data.List (nub)

{- **********************************************************
   * Data type for a regular expression.                    *
   **********************************************************
-}

data Reg a = Empty              | -- []
	     Epsilon            | -- 0
	     All                | -- ?
	     Symbol a           | -- a
	     Reg a :|: Reg a    | -- [ r1 | r2 ]
	     Reg a :.: Reg a    | -- [ r1 r2 ]
	     Reg a :&: Reg a    | -- [ r1 & r2 ]
	     Complement (Reg a) | -- ~[ r1 ]
	     Star       (Reg a)   -- [ r2 ]*
	deriving (Eq)

{- **********************************************************
   * Combinators.                                           *
   * The regular expressions are simplified while combined. *
   **********************************************************
-}

infixl 5  |>  -- Concatenation
infixl 4 <|>  -- Union
infixl 3 <&>  -- Intersection
infixl 3 <->  -- Set minus

class Combinators a where
 (<|>) :: a -> a -> a -- Union
 (|>)  :: a -> a -> a -- Concatenation
 star  :: a -> a      -- Kleene's star
 plus  :: a -> a      -- Kleene's plus
 empty :: a

instance Eq a => Combinators (Reg a) where
 Empty <|> b = b                    -- [ [] | r1 ] = r1
 a <|> Empty = a                    -- [ r1 | [] ] = r1
 _ <|> (Star All) = Star All
 (Star All) <|> _ = Star All
 a1@(a :.: b) <|> a2@(c :.: d)
  | a1 == a2  = a1
  | a == c    = a |> (b <|> d)
  | b == d    = (a <|> c) |> b
  | otherwise = a1 :|: a2
 a <|> b
  | a == b = a                      -- [ r1 | r1 ] = r1
  | otherwise = a :|: b

 Empty |> _   = empty               -- [ [] r1 ] = []
 _ |> Empty   = empty               -- [ r1 [] ] = []
 Epsilon |> b = b                   -- [ 0 r1 ]  = r1
 a |> Epsilon = a                   -- [ r1 0 ]  = r1
 a |> b       = a :.: b

 star (Star a)  = star a            -- [r1]**  = [r1]*
 star (Epsilon) = eps               -- [0]*    = 0
 star (Empty)   = eps               -- [ [] ]* = 0
 star a         = Star a

 plus a         = a |> star a

 empty = Empty

{- Intersection -}

(<&>) :: Eq a => Reg a -> Reg a -> Reg a
_ <&> Empty = Empty                 -- [ r1 & [] ] = []
Empty <&> _ = Empty                 -- [ [] & r1 ] = []
(Star All) <&> a = a
a <&> (Star All) = a
a <&> b
 | a == b    = a                    -- [ r1 & r1 ] = r1
 | otherwise = a :&: b

{- Minus. Definition A - B = A & ~B -}

(<->) :: Eq a => Reg a -> Reg a -> Reg a
Empty <-> _ = empty                 -- [ [] - r1 ] = []
a <-> Empty = a                     -- [ r1 - [] ] = r1
a <-> b
 | a == b    = empty                -- [ r1 - r1 ] = []
 | otherwise = a <&> (complement b)

s :: a -> Reg a
s a = Symbol a

eps :: Reg a
eps = Epsilon

allS :: Reg a
allS = All

complement :: Eq a => Reg a -> Reg a
complement Empty   = star allS       -- ~[ [] ] = ?*
complement Epsilon = plus allS       -- ~[ 0 ] = [? ?*]
complement (Star All) = empty
complement (Complement a) = a
complement a       = Complement a

{- *******************************************************************
   * allToSymbols:  ? -> [a|..] with respect to an alphabet [a]      *
   * allFreeReg: Construct a ?-free regular expression with respect  *
   *             to an alphabet [a]                                  *
   *******************************************************************
-}

allToSymbols :: Eq a => [a] -> Reg a
allToSymbols sigma  = case sigma of
 [] -> empty
 ys -> foldr1 (:|:) [s a| a <- ys]

allFree :: Eq a => Reg a -> [a] -> Reg a
allFree (a :|: b)      sigma  = (allFree a sigma) :|: (allFree b sigma)
allFree (a :.: b)      sigma  = (allFree a sigma) :.: (allFree b sigma)
allFree (a :&: b)      sigma  = (allFree a sigma) :&: (allFree b sigma)
allFree (Complement a) sigma  = Complement (allFree a sigma)
allFree (Star a)       sigma  = Star       (allFree a sigma)
allFree (All)          sigma  = allToSymbols sigma
allFree r                  _  = r

{- **********************************************************
   * reversal: reverse the language denoted by the regular  *
   *           expression.                                  *
   **********************************************************
-}

reversal :: Eq a => Reg a -> Reg a
reversal (a :|: b)      = (reversal a) :|: (reversal b)
reversal (a :.: b)      = (reversal b) :.: (reversal a)
reversal (a :&: b)      = (reversal a) :&: (reversal b)
reversal (Complement a) = Complement (reversal a)
reversal (Star a)       = Star (reversal a)
reversal r              = r

{- ***********************************************************
   * acceptEps: Examines if a regular expression accepts     *
   *            the empty string.                            *
   ***********************************************************
-}

acceptEps :: Eq a => Reg a -> Bool
acceptEps (Epsilon)             = True
acceptEps (Star _)              = True
acceptEps (a :|: b)             = acceptEps a || acceptEps b
acceptEps (a :.: b)             = acceptEps a && acceptEps b
acceptEps (a :&: b)             = acceptEps a && acceptEps b
acceptEps (Complement a)        = not (acceptEps a)
acceptEps _                     = False

{- **********************************************************
   * Symbols: type class for the collection of symbols in a *
   * expression.                                            *
   **********************************************************
-}

class Symbols f where
 symbols :: Eq a => f a -> [a]

instance Symbols Reg  where
 symbols (Symbol a)          = [a]
 symbols (a :.: b)           = nub $ (symbols a) ++ (symbols b)
 symbols (a :|: b)           = nub $ (symbols a) ++ (symbols b)
 symbols (a :&: b)           = nub $ (symbols a) ++ (symbols b)
 symbols (Complement a)      = symbols a
 symbols (Star a)            = symbols a
 symbols _                   = []

{- **********************************************************
   * Instance of Show (Reg a)                               *
   **********************************************************
-}

instance Show a => Show (Reg a) where
 show (Empty)        = "[0 - 0]"
 show (Epsilon)      = "0"
 show (Symbol a)     = show a
 show (All)          = "?"
 show (Complement a) = "~" ++ "[" ++ show a ++ "]"
 show (Star a)       = "[" ++ show a ++ "]* "
 show (a :|: b)      = "[" ++ show a ++ " | " ++ show b ++ "]"
 show (a :.: b)      = "[" ++ show a ++ " "   ++ show b ++ "]"
 show (a :&: b)      = "[" ++ show a ++ " & " ++ show b ++ "]"
