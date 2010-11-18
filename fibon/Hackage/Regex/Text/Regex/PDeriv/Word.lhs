> module Text.Regex.PDeriv.Word where

> -- | the Word type class
> class Word a where
>     uncons :: a -> Maybe (Char,a)
>     take :: Int -> a -> a
>     drop :: Int -> a -> a
>     empty :: a
>     reverse :: a -> a
>     append :: a -> a -> a
>     length :: a -> Int

