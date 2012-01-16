{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.BitmapInfoV3
	( BitmapInfoV3	(..)
	, Compression (..)
	, sizeOfBitmapInfoV3
	, checkBitmapInfoV3
        , imageSizeFromBitmapInfoV3)
where
import Codec.BMP.Error
import Codec.BMP.Compression
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
	  --   Some encoders set this to zero, so we need to calculate it based on the
	  --   overall file size.
	  -- 
	  --   If it is non-zero then we check it matches the file size - header size.
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
	
		
-- | Check headers for problems and unsupported features.	 
checkBitmapInfoV3 :: BitmapInfoV3 -> Word32 -> Maybe Error
checkBitmapInfoV3 header physicalBufferSize

        -- We only handle a single color plane.
	| dib3Planes header /= 1
	= Just	$ ErrorUnhandledPlanesCount $ dib3Planes header
	
        -- We only handle 24 and 32 bit images.
	| dib3BitCount header /= 24
        , dib3BitCount header /= 32
	= Just 	$ ErrorUnhandledColorDepth $ dib3BitCount header

        -- If the image size field in the header is non-zero, 
        -- then it must be the same as the physical size of the image buffer.
        | headerImageSize               <- dib3ImageSize header
        , headerImageSize /= 0
        , fromIntegral headerImageSize /= physicalBufferSize
        = Just  $ ErrorImagePhysicalSizeMismatch
                        headerImageSize physicalBufferSize

        -- Check that the physical buffer contains enough image data.
        -- It may contain more, as some encoders put padding bytes
        -- on the end.
        | Just calculatedImageSize      <- imageSizeFromBitmapInfoV3 header
        , fromIntegral physicalBufferSize < calculatedImageSize
        = Just  $ ErrorImageDataTruncated 
                        calculatedImageSize
                        (fromIntegral physicalBufferSize)

        -- We only handle uncompresssed images.
        | dib3Compression header /= CompressionRGB
        = Just  $ ErrorUnhandledCompressionMode (dib3Compression header)

	| otherwise
	= Nothing
	

-- | Compute the size of the image data from the header.
--
--   * We can't just use the 'dib3ImageSize' field because some encoders
--     set this to zero.
--
--   * We also can't use the physical size of the data in the file because
--     some encoders add zero padding bytes on the end.   
--
imageSizeFromBitmapInfoV3 :: BitmapInfoV3 -> Maybe Int
imageSizeFromBitmapInfoV3 header
        | dib3BitCount    header == 32
        , dib3Planes      header == 1
        , dib3Compression header == CompressionRGB
        = Just $ fromIntegral (dib3Width header * dib3Height header * 4)

        | dib3BitCount    header == 24
        , dib3Planes      header == 1
        , dib3Compression header == CompressionRGB
        = let   imageBytesPerLine = dib3Width header * 3
                tailBytesPerLine  = imageBytesPerLine `mod` 4
                padBytesPerLine   = if tailBytesPerLine > 0
                                        then 4 - tailBytesPerLine
                                        else 0
          in    Just $ fromIntegral (dib3Height header * imageBytesPerLine + padBytesPerLine)

        | otherwise
        = Nothing

