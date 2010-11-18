{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.FileHeader
	( FileHeader	(..)
	, bmpMagic
	, sizeOfFileHeader
	, checkFileHeader)
where
import Codec.BMP.BitmapInfoV3
import Codec.BMP.Error
import Data.Binary
import Data.Binary.Get	
import Data.Binary.Put
	
-- File Headers -----------------------------------------------------------------------------------
-- | BMP file header.
data FileHeader
	= FileHeader			
	{ -- | (+0) Magic numbers 0x42 0x4d
	  fileHeaderType	:: Word16
	
	  -- | (+2) Size of the file, in bytes.
	, fileHeaderFileSize	:: Word32

	  -- | (+6) Reserved, must be zero.
	, fileHeaderReserved1	:: Word16

	  -- | (+8) Reserved, must be zero.
	, fileHeaderReserved2	:: Word16

	  -- | (+10) Offset in bytes to the start of the pixel data.
	, fileHeaderOffset	:: Word32
	}
	deriving (Show)

-- | Size of a file header (in bytes).
sizeOfFileHeader :: Int
sizeOfFileHeader = 14

-- | Magic number that should come at the start of a BMP file.
bmpMagic :: Word16
bmpMagic = 0x4d42


instance Binary FileHeader where
 get 
  = do	t	<- getWord16le
	size	<- getWord32le
	res1	<- getWord16le
	res2	<- getWord16le
	offset	<- getWord32le
	
	return	$ FileHeader
		{ fileHeaderType	= t
		, fileHeaderFileSize	= size
		, fileHeaderReserved1	= res1
		, fileHeaderReserved2   = res2
		, fileHeaderOffset	= offset }

 put header
  = do	putWord16le	$ fileHeaderType header
	putWord32le	$ fileHeaderFileSize header
	putWord16le	$ fileHeaderReserved1 header
	putWord16le	$ fileHeaderReserved2 header
	putWord32le	$ fileHeaderOffset header
	

-- | Check a file header for problems and unsupported features.
checkFileHeader :: FileHeader -> Maybe Error	
checkFileHeader header
	| fileHeaderType header /= bmpMagic
	= Just	$ ErrorBadMagic (fileHeaderType header)

	| fileHeaderFileSize header 
		< fromIntegral (sizeOfFileHeader + sizeOfBitmapInfoV3)
	= Just	$ ErrorDodgyFileHeaderFieldFileSize 
		$ fromIntegral $ fileHeaderFileSize header

	| fileHeaderReserved1 header /= 0
	= Just 	$ ErrorReservedFieldNotZero

	| fileHeaderReserved2 header /= 0
	= Just 	$ ErrorReservedFieldNotZero

	| fromIntegral (fileHeaderOffset header) /= sizeOfFileHeader + sizeOfBitmapInfoV3
	= Just	$ ErrorDodgyFileHeaderFieldOffset
		$ fromIntegral $ fileHeaderOffset header

	| otherwise
	= Nothing