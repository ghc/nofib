{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.Base
	(BMP	(..))
where
import Codec.BMP.FileHeader
import Codec.BMP.BitmapInfo
import Data.ByteString

-- | A BMP image.
--	For an uncompressed image, the image data contains triples of BGR component values.
--	Each line may also have zero pad values on the end, to bring them up to a multiple
--	of 4 bytes in length.
data BMP
	= BMP
	{ bmpFileHeader		:: FileHeader
	, bmpBitmapInfo		:: BitmapInfo
	, bmpRawImageData	:: ByteString }
	deriving Show





