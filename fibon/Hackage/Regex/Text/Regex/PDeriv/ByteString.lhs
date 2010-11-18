> {- By Kenny Zhuo Ming Lu and Martin Sulzmann, 2009, BSD License -}

A bytestring implementation of reg exp pattern matching using partial derivative

> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies,
>     FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-} 


> module Text.Regex.PDeriv.ByteString 
>     ( Regex
>     , CompOption(..)
>     , ExecOption(..)
>     , defaultCompOpt
>     , defaultExecOpt
>     , compile
>     , execute
>     , regexec
>     ) where 

The re-exports

> import Text.Regex.PDeriv.ByteString.RightToLeft ( Regex
>                                                 , CompOption(..)
>                                                 , ExecOption(..)
>                                                 , defaultCompOpt
>                                                 , defaultExecOpt
>                                                 , compile
>                                                 , execute
>                                                 , regexec
>                                                 ) 

