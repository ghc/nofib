{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.BitmapInfoV4
	( BitmapInfoV4	(..)
	, CIEXYZ        (..)
	, sizeOfBitmapInfoV4
	, checkBitmapInfoV4)
where
import Codec.BMP.Error
import Codec.BMP.CIEXYZ
import Codec.BMP.BitmapInfoV3
import Data.Binary
import Data.Binary.Get	
import Data.Binary.Put

-- | Device Independent Bitmap (DIB) header for Windows V4 (95 and newer)
data BitmapInfoV4
	= BitmapInfoV4
	{ -- | Size of the image header, in bytes.
	  dib4InfoV3		:: BitmapInfoV3

	  -- | Color masks specify components of each pixel.
	  --   Only used with the bitfields compression mode.
	, dib4RedMask		:: Word32
	, dib4GreenMask		:: Word32
	, dib4BlueMask		:: Word32
	, dib4AlphaMask		:: Word32

	-- | The color space used by the image.
	, dib4ColorSpaceType	:: Word32

	-- | Specifies the XYZ coords of the three colors that correspond to the RGB endpoints
	--   for the logical color space associated with the bitmap. 
	--   Only used when ColorSpaceType specifies a calibrated image.
	, dib4Endpoints		:: (CIEXYZ, CIEXYZ, CIEXYZ)

	-- | Toned response curves for each component. 
	--   Only used when the ColorSpaceType specifies a calibrated image.
	, dib4GammaRed		:: Word32
	, dib4GammaGreen	:: Word32
	, dib4GammaBlue		:: Word32
	}
	deriving (Show)


-- | Size of `BitmapInfoV4` header (in bytes)
sizeOfBitmapInfoV4 :: Int
sizeOfBitmapInfoV4 = 108


instance Binary BitmapInfoV4 where
 get
  = do	infoV3	<- get
	rmask	<- getWord32le
	gmask	<- getWord32le
	bmask	<- getWord32le
	amask	<- getWord32le
	cstype	<- getWord32le
	ends	<- get
	rgamma	<- getWord32le
	ggamma	<- getWord32le
	bgamma	<- getWord32le
	
	return	$ BitmapInfoV4
		{ dib4InfoV3		= infoV3
		, dib4RedMask		= rmask
		, dib4GreenMask		= gmask
		, dib4BlueMask		= bmask
		, dib4AlphaMask		= amask
		, dib4ColorSpaceType	= cstype
		, dib4Endpoints		= ends
		, dib4GammaRed		= rgamma
		, dib4GammaGreen	= ggamma
		, dib4GammaBlue		= bgamma }
		

 put header
  = do	put		$ dib4InfoV3		header
	putWord32le	$ dib4RedMask		header
	putWord32le	$ dib4GreenMask		header
	putWord32le	$ dib4BlueMask		header
	putWord32le	$ dib4AlphaMask		header
	putWord32le	$ dib4ColorSpaceType	header
	put		$ dib4Endpoints 	header
	putWord32le	$ dib4GammaRed		header
	putWord32le	$ dib4GammaGreen	header
	putWord32le	$ dib4GammaBlue		header


	
-- | Check headers for problems and unsupported features.	 
--	With a V4 header we support both the uncompressed 24bit RGB format,
--	and the uncompressed 32bit RGBA format.
--
checkBitmapInfoV4 :: BitmapInfoV4 ->  Maybe Error
checkBitmapInfoV4 headerV4
		
	| dib3Planes headerV3 /= 1
	= Just	$ ErrorUnhandledPlanesCount 
		$ fromIntegral $ dib3Planes headerV3
	
	| dib3ImageSize headerV3 == 0
	= Just	$ ErrorZeroImageSize
	
	| dib3ImageSize headerV3 `mod` dib3Height headerV3 /= 0
	= Just	$ ErrorLacksWholeNumberOfLines

	-- Check for valid compression modes ----

	-- uncompressed 24bit RGB
	| dib3BitCount    headerV3 == 24 
	, dib3Compression headerV3 == CompressionRGB
	= Nothing
	
	-- uncompressed 32bit RGBA
	| dib3BitCount    headerV3 == 32
	, dib3Compression headerV3 == CompressionBitFields
	, dib4AlphaMask   headerV4 == 0xff000000
	, dib4RedMask     headerV4 == 0x00ff0000
	, dib4GreenMask   headerV4 == 0x0000ff00
	, dib4BlueMask    headerV4 == 0x000000ff
	= Nothing
	
	-- Some unsupported compression mode ----
	| otherwise
	= Just $ ErrorUnhandledCompressionMode
	
	where	headerV3 = dib4InfoV3 headerV4
	
	