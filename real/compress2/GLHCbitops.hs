module GLHCbitops
where

infixl 8 `intLsh`, `intRsh`
infixl 7 `intAnd`
infixl 5 `intOr`

intLsh (MkInt x) (MkInt y) 
      = case shiftL#  x y of z -> MkInt z
intRsh (MkInt x) (MkInt y) 
      = case shiftR#  x y of z -> MkInt z
intAnd (MkInt x) (MkInt y) 
      = case andInt#  x y of z -> MkInt z
intOr (MkInt x) (MkInt y) 
      = case orInt#  x y of z -> MkInt z
