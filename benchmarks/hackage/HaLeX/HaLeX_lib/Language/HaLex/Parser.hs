--
-- Parser Combinators Library
--        (Utrecht University style) 
--
-- Code Included in the Lecture Notes on
-- 
--      Language Processing (with a functional flavour)             
--
--
-- copyright João Saraiva
--           Department of Computer Science,
--           University of Minho, 
--           Braga, Portugal
--           jas@di.uminho.pt
--           2001
--

module Language.HaLex.Parser where

infixl 3 <|> ; infixl 4 <*>


--
-- The type of a (sub) Parser Function
--

type Parser s r = [s] -> [(r,[s])]


--
-- Elementary Parser Combinators
--

symbol :: Eq a => a -> Parser a a 
symbol _ []                 = []
symbol s (x:xs) | x == s    = [(s,xs)]
                | otherwise = []

satisfy :: (s -> Bool) -> Parser s s
satisfy p []                 = []
satisfy p (x:xs) | p x       = [(x,xs)]
                 | otherwise = []

token :: Eq s => [s] -> Parser s [s]
token k xs | k == take n xs = [(k,drop n xs)]
           | otherwise       = []
 where n = length k

succeed :: a -> Parser s a
succeed r xs = [(r,xs)]

--
-- Basic Parser Combinators
--

(<|>) :: Parser s a -> Parser s a -> Parser s a
(p <|> q) xs = p xs ++ q xs


(p <*> q) xs = [ ( f z , zs )
               | ( f   , ys ) <- p xs
               , (   z , zs ) <- q ys 
               ]

(f <$> p) xs = [(f y, ys) | (y,ys) <- p xs ]




