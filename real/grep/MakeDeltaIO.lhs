> module Main where

mkYesFun i
makes the definition of a function qy_i of type 
qy_i :: (String -> Bool) -> (Char -> String -> Bool) -> String -> Bool

> mkYesFun :: Int -> Dialogue -> Dialogue
> mkYesFun i = output (
> 	"qy" ++ show i ++ " y n ('\\" ++ show i ++ "': cs) = y cs\n\
>	\qy" ++ show i ++ " y n (c:cs) = n c cs\n\
>	\qy" ++ show i ++ " y n [] = False\n")

mkNoFun i
makes the definition of a function of type
qn_i :: (String -> Bool) -> (Char -> String -> Bool) -> Char -> String -> Bool

> mkNoFun :: Int -> Dialogue -> Dialogue
> mkNoFun i = output
> 	("qn" ++ show i ++ " y n c cs = if c == '\\" ++ show i ++"' then y cs else n c cs\n")

mkFuns
provides an iterator for the above two functions

> mkFuns :: (Int -> Dialogue -> Dialogue) -> Int -> Int -> Dialogue -> Dialogue
> mkFuns fun f t = do f
>   where
>     do i = if i <= t then fun i . do (i+1) else id

mkFunList name f t
*** unused ***
creates the definition for a list <name>s which contains all of the <name>_i in order
<name>s :: [(String -> Bool) -> (String -> Bool) -> String -> Bool]

> mkFunList :: String -> Int -> Int -> Dialogue -> Dialogue
> mkFunList name f t = output
> 	(name ++ "s = [" ++ tail (concat [ ",\n\t" ++ name ++ show i | i <- [f..t]]) ++ "]\n")

mkSelect name i
creates the i-th branch for the function <name>fun of type
<name>fun :: Char -> typeOf <name>

> mkSelect :: String -> Int -> Dialogue -> Dialogue
> mkSelect name i = output
> 	(name ++ "fun '\\" ++ show i ++ "' = " ++ name ++ show i ++ "\n")


mkArray f t
*** unused ***
creates the definition for the array qa which contains all of the q_i
qa :: Array Int ((String -> Bool) -> (String -> Bool) -> String -> Bool)

> mkArray :: Int -> Int -> Dialogue -> Dialogue
> mkArray f t = output
> 	("qa = listArray ('\\" ++ show f ++ "','\\" ++ show t ++"') qs\n")

> mkFinal = output(
>    	"final :: String -> Bool\n\
>	\final _ = True\n")

> mkHeader = output "module Delta (qyfun, qnfun, final) where\n"

> main = (
> 	mkHeader .
>	mkFinal .
>	mkFuns (mkSelect "qy") 0 127 .
>	mkFuns (mkSelect "qn") 0 127 .
>	mkFuns mkYesFun 0 127 .
>	mkFuns mkNoFun 0 127
>	) done

> output :: String -> Dialogue -> Dialogue
> output str = appendChan stdout str exit

