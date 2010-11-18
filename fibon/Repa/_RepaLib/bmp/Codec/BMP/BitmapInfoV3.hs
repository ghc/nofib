{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.BitmapInfoV3
	( BitmapInfoV3	(..)
	, Compression (..)
	, sizeOfBitmapInfoV3
	, checkBitmapInfoV3)
where
import Codec.BMP.Error
import Data.Binary
import Data.Binary.Get	
import Data.Binary.Put

-- | Device Independent Bitmap (DIB) header for Windows V3.
data BitmapInfoV3
	= BitmapInfoV3			
	{ -- | (+0) Size of the image header, in bytes.
	  dib3Size		:: Word32

	  -- | (+4) Width of the image, in pixels.
	, dib3Width		:: Word32
	
	  -- | (+8) Height of the image, in pixels.
	, dib3Height		:: Word32
	
	  -- | (+12) Number of color planes.
	, dib3Planes		:: Word16

	  -- | (+14) Number of bits per pixel.
	, dib3BitCount		:: Word16

	  -- | (+16) Image compression mode.
	, dib3Compression	:: Compression

	  -- | (+20) Size of raw image data.
	, dib3ImageSize		:: Word32

	  -- | (+24) Prefered resolution in pixels per meter, along the X axis.
	, dib3PelsPerMeterX	:: Word32

	  -- | (+28) Prefered resolution in pixels per meter, along the Y axis.
	, dib3PelsPerMeterY	:: Word32

	  -- | (+32) Number of color entries that are used.
	, dib3ColorsUsed	:: Word32

	  -- | (+36) Number of significant colors.
	, dib3ColorsImportant	:: Word32
	}
	deriving (Show)

data Compression
	= CompressionRGB
	| CompressionRLE8
	| CompressionRLE4
	| CompressionBitFields
	| CompressionJPEG
	| CompressionPNG
	| CompressionUnknown Word32
	deriving (Show, Eq)


-- | Size of `BitmapInfoV3` header (in bytes)
sizeOfBitmapInfoV3 :: Int
sizeOfBitmapInfoV3 = 40


instance Binary BitmapInfoV3 where
 get
  = do	size	<- getWord32le
	width	<- getWord32le
	height	<- getWord32le
	planes	<- getWord16le
	bitc	<- getWord16le
	comp	<- get
	imgsize	<- getWord32le
	pelsX	<- getWord32le
	pelsY	<- getWord32le
	cused	<- getWord32le
	cimp	<- getWord32le
	
	return	$ BitmapInfoV3
		{ dib3Size		= size
		, dib3Width		= width
		, dib3Height		= height
		, dib3Planes		= planes
		, dib3BitCount		= bitc
		, dib3Compression	= comp
		, dib3ImageSize		= imgsize
		, dib3PelsPerMeterX	= pelsX
		, dib3PelsPerMeterY	= pelsY
		, dib3ColorsUsed	= cused
		, dib3ColorsImportant	= cimp }

 put header
  = do	putWord32le 	$ dib3Size header
	putWord32le	$ dib3Width header
	putWord32le	$ dib3Height header
	putWord16le	$ dib3Planes header
	putWord16le	$ dib3BitCount header
	put		$ dib3Compression header
	putWord32le	$ dib3ImageSize header
	putWord32le	$ dib3PelsPerMeterX header
	putWord32le	$ dib3PelsPerMeterY header
	putWord32le	$ dib3ColorsUsed header
	putWord32le	$ dib3ColorsImportant header
	
	
instance Binary Compression where
 get
  = do	c	<- getWord32le
	case c of
	 0	-> return $ CompressionRGB
	 1	-> return $ CompressionRLE8
	 2	-> return $ CompressionRLE4
	 3	-> return $ CompressionBitFields
	 4	-> return $ CompressionJPEG
	 5	-> return $ CompressionPNG
	 _	-> return $ CompressionUnknown c
	
 put c
  = case c of
	CompressionRGB		-> putWord32le 0
	CompressionRLE8		-> putWord32le 1
	CompressionRLE4		-> putWord32le 2
	CompressionBitFields	-> putWord32le 3
	CompressionJPEG		-> putWord32le 4
	CompressionPNG		-> putWord32le 5
	CompressionUnknown x	-> putWord32le x
	
	
-- | Check headers for problems and unsupported features.	 
--	With a V3 header we only support the uncompressed 24bit RGB format.
checkBitmapInfoV3 :: BitmapInfoV3 ->  Maybe Error
checkBitmapInfoV3 header
		
	| dib3Planes header /= 1
	= Just	$ ErrorUnhandledPlanesCount 
		$ fromIntegral $ dib3Planes header
	
	| dib3ImageSize header == 0
	= Just	$ ErrorZeroImageSize
	
	| dib3ImageSize header `mod` dib3Height header /= 0
	= Just	$ ErrorLacksWholeNumberOfLines

	| dib3BitCount header /= 24
	= Just 	$ ErrorUnhandledColorDepth
		$ fromIntegral $ dib3BitCount header

	| dib3Compression header /= CompressionRGB
	= Just	$ ErrorUnhandledCompressionMode
	
	| otherwise
	= Nothing
	

