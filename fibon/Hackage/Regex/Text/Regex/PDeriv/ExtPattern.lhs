> module Text.Regex.PDeriv.ExtPattern where

> -- | The external pattern syntax (ERE syntax)
> data EPat = EEmpty 
>          | EGroup EPat    -- ^ the group ( re )
>          | EOr [EPat]     -- ^ the union re|re
>          | EConcat [EPat] -- ^ the concantenation rere
>          | EOpt EPat Bool -- ^ the option re?, the last boolean flag indicates whether it is greedy
>          | EPlus EPat Bool -- ^ the plus re+
>          | EStar EPat Bool -- ^ the star re*
>          | EBound EPat Int (Maybe Int) Bool -- ^ re{1:10}
>          | ECarat         -- ^ the ^ NOTE:shouldn't this must be top level?
>          | EDollar        -- ^ the $
>          | EDot           -- ^ the any char .
>          | EAny [Char]    -- ^ the character class [ a-z ] 
>          | ENoneOf [Char] -- ^ the negative character class [^a-z]
>          | EEscape Char   -- ^ backslash char
>          | EChar Char     -- ^ the non-escaped char
>           deriving Show

> -- | Function 'hasGroup' tests whether an external pattern has ( ... ) (i.e. variable patterns in the internal pattern)
> hasGroup :: EPat -> Bool
> hasGroup EEmpty = False
> hasGroup (EGroup _) = True
> hasGroup (EOr eps) = any hasGroup eps
> hasGroup (EConcat eps) = any hasGroup eps
> hasGroup (EOpt ep _) = hasGroup ep
> hasGroup (EPlus ep _) = hasGroup ep
> hasGroup (EStar ep _) = hasGroup ep
> hasGroup (EBound ep _ _ _) = hasGroup ep
> hasGroup ECarat = False
> hasGroup EDollar = False
> hasGroup EDot = False
> hasGroup (EAny _) = False
> hasGroup (ENoneOf _) = False
> hasGroup (EEscape _) = False
> hasGroup (EChar _) = False