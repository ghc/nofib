-- mimic "hbc_library" module, Word.
-- [seriously non-std Haskell here]
--
module Word (
	Bits(..),		-- class
	Byte, Short, Word,	-- data types: abstract
	byteToInt, shortToInt, wordToInt
    ) where

infixl 8 `bitLsh`, `bitRsh`
infixl 7 `bitAnd`
infixl 6 `bitXor`
infixl 5 `bitOr`

class Bits a where
	bitAnd, bitOr, bitXor :: a -> a -> a
	bitCompl :: a -> a
	bitRsh, bitLsh :: a -> Int -> a
	bitSwap :: a -> a
	bit0 :: a
	bitSize :: a -> Int

------------------------------------------------------------------
data Word = Word Int# deriving (Eq, Ord)

instance Bits Word where
	bitAnd (Word x) (Word y) = case andInt# x y of z -> Word z
	bitOr  (Word x) (Word y) = case orInt#  x y of z -> Word z
	bitXor (Word x) (Word y) = error "later..." -- Word (XOR x y)
	bitCompl (Word x)        = case notInt# x of x' -> Word x'
	bitLsh (Word x) (MkInt y)= case shiftL# x y of z -> Word z
	bitRsh (Word x) (MkInt y)= case shiftR# x y of z -> Word z
        bitSwap (Word x)         = --Word (OR (LSH x 16) (AND (RSH x 16) 65535))
				   case shiftL# x  16# of { a# ->
				   case shiftR# x  16# of { b# ->
				   case andInt# b# 65535# of { c# ->
				   case orInt#  a# c# of  { r# ->
				   Word r# }}}}
	bit0                     = Word 1#
	bitSize (Word _)	 = 32

instance Num Word where
	Word x + Word y = case plusInt#  x y of z -> Word z
	Word x - Word y = case minusInt# x y of z -> Word z
	Word x * Word y = case timesInt# x y of z -> Word z
	negate (Word x) = case negateInt# x  of z -> Word z
	fromInteger (MkInteger a# s# d#)
	  = case integer2Int# a# s# d# of { z# ->
	    Word z# }

instance Text Word where
	showsPrec _ (Word w) =
		let i = toInteger (MkInt w) + (if w >=# 0# then 0 else  2*(toInteger maxInt + 1))
		in  showString (conv 8 i)

conv :: Int -> Integer -> String
conv 0 _ = ""
conv n i = conv (n-1) q ++ ["0123456789ABCDEF"!!r] where (q, r) = quotRem i 16

------------------------------------------------------------------
data Short = Short Int# deriving (Eq, Ord)

------------------------------------------------------------------
data Byte = Byte Int# deriving (Eq, Ord)

------------------------------------------------------------------
wordToInt :: Word -> Int
wordToInt (Word w) = MkInt w

shortToInt :: Short -> Int
shortToInt (Short w) = MkInt w

byteToInt :: Byte -> Int
byteToInt (Byte w) = MkInt w
