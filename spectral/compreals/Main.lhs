\chapter{Command Interpreter}

\begin{verbatim}
$Log: Main.lhs,v $
Revision 1.2  1996/07/25 21:30:47  partain
Bulk of final changes for 2.01

Revision 1.1  1996/01/08 20:05:20  partain
Initial revision

\end{verbatim}

Eventually we will build a command interpreter here, for a desk
calculator type language.

> module Main where
> import RealReals

For the moment on the other hand it just prints a given number.

> main = getContents >>= foldr output . map doLine . lines

Printing out @String@'s is easy:

> output :: String -> IO () -> IO ()
> output string dialogue = putStr (string++"\n") >> dialogue

The @doLine@ function parses the line, if it is syntactically correct
it then evaluates the expression returning the answer string.

> doLine :: String -> String
> doLine = eval [] . tokenize

> tokenize ""                 = []
> tokenize (c:cs) | isSpace c = tokenize (dropWhile isSpace cs)
> tokenize (c:cs) | isSymb  c = [c]: tokenize cs
> tokenize (c:cs) | isAlpha c = case (span isAlphaNum cs) of
>                                (nam,t) -> (c:nam): tokenize t
> tokenize (c:cs) | isDigit c = case (span isDigit cs)  of
>                                (num,t) -> (c:num): tokenize t
> tokenize _                  = ["Error"]

> isSymb c = c `elem` "*+-/"
> isAlphaNum c = isAlpha c || isDigit c

> eval :: [RealReal] -> [String] -> String
> eval [n] []     = show n
> eval ns (t:ts) | isSymb  (head t)
>  = case head t of
>     '+' -> check2 (+) ns ts
>     '-' -> check2 (-) ns ts
>     '*' -> check2 (*) ns ts
>     '/' -> check2 (/) ns ts
>     _   -> "Error"
> eval ns (t:ts) | isDigit (head t)
>  = eval (fromInteger (parseInteger t): ns) ts
> eval ns (t:ts) | isAlpha (head t)
>  = case t of
>      "abs"    -> check1 abs    ns ts
>      "signum" -> check1 signum ns ts
>      "pi"     -> eval  (pi:ns)    ts
>      "exp"    -> check1 exp    ns ts
>      "log"    -> check1 log    ns ts
>      "sqrt"   -> check1 sqrt   ns ts
>      "sin"    -> check1 sin    ns ts
>      "cos"    -> check1 cos    ns ts
>      "tan"    -> check1 tan    ns ts
>      "asin"   -> check1 asin   ns ts
>      "acos"   -> check1 acos   ns ts
>      "atan"   -> check1 atan   ns ts
>      "sinh"   -> check1 sinh   ns ts
>      "cosh"   -> check1 cosh   ns ts
>      "tanh"   -> check1 tanh   ns ts
>      "asinh"  -> check1 asinh  ns ts
>      "acosh"  -> check1 acosh  ns ts
>      "atanh"  -> check1 atanh  ns ts
>      _        -> "Error"
> eval _  _ = "Error"

> check1 :: (RealReal -> RealReal) -> [RealReal] -> [String] -> String
> check1 f (n:ns) ts = eval (f n: ns) ts
> check1 f _      ts = "Error"

> check2 :: (RealReal -> RealReal -> RealReal) ->
>           [RealReal] -> [String] -> String
> check2 f (n0:n1:ns) ts = eval (f n1 n0 : ns) ts
> check2 f _          ts = "Error"

> parseInteger :: String -> Integer
> parseInteger = makeNumber 10 . map number
>                where number :: Char -> Integer
>                      number c = toInteger (fromEnum c - fromEnum '0')

> makeNumber :: Integer -> [Integer] -> Integer
> makeNumber m = foldl f 0
>                where f a x = a * m + x

