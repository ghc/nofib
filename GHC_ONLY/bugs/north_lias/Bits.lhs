% Bits.lhs - useful extras for testing LIAS

> module Bits (
>     Cont(..),
>     showit, showits, new_line, pad
>     ) where

> type Cont  =  Dialogue

> showit :: (Text a) => a -> Cont -> Cont
> showit x c  =  appendChan stdout (show x) exit c

> showits :: String -> Cont -> Cont
> showits x c  =  appendChan stdout x exit c

> new_line :: Cont -> Cont
> new_line  =  showits "\n"

> pad :: Int -> String -> String
> pad n cs  =  take (n - length cs) (repeat ' ') ++ cs
