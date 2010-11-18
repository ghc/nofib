{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.BitmapInfoV5
	( BitmapInfoV5	(..)
	, sizeOfBitmapInfoV5
	, checkBitmapInfoV5)
where
import Codec.BMP.Error
import Codec.BMP.BitmapInfoV4
import Data.Binary
import Data.Binary.Get	
import Data.Binary.Put


-- | Device Independent Bitmap (DIB) header for Windows V5 (98/2000 and newer)
data BitmapInfoV5
	= BitmapInfoV5
	{ dib5InfoV4		:: BitmapInfoV4
	
	-- | Rendering intent for the bitmap.
	, dib5Intent		:: Word32

	-- | Offset (in bytes) from the beginning of the header to the start of the profile data.
	, dib5ProfileData	:: Word32

	-- | Size (in bytes) of embedded profile data.
	, dib5ProfileSize	:: Word32
	
	-- | Reserved, should be zero.
	, dib5Reserved		:: Word32
	}
	deriving (Show)

-- | Size of `BitmapInfoV5` header (in bytes)
sizeOfBitmapInfoV5 :: Int
sizeOfBitmapInfoV5 = 124


instance Binary BitmapInfoV5 where
 get
  = do	infoV4	<- get
	intent	<- getWord32le
	pdata	<- getWord32le
	psize	<- getWord32le
	res	<- getWord32le
	
	return	$ BitmapInfoV5
		{ dib5InfoV4		= infoV4
		, dib5Intent		= intent
		, dib5ProfileData	= pdata
		, dib5ProfileSize	= psize
		, dib5Reserved		= res }
		

 put header
  = do	put		$ dib5InfoV4		header
	putWord32le	$ dib5Intent		header
	putWord32le	$ dib5ProfileData	header
	putWord32le	$ dib5ProfileSize	header
	putWord32le	$ dib5Reserved		header

	
-- | Check headers for problems and unsupported features.	 
--	The V5 header doesn't give us any more useful info than the V4 one.
checkBitmapInfoV5 :: BitmapInfoV5 ->  Maybe Error
checkBitmapInfoV5 header
	= checkBitmapInfoV4 $ dib5InfoV4 header


