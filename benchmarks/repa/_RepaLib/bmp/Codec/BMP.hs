{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

-- | Reading and writing uncompressed BMP files.
--
--   Reading works for both uncompressed 24bit RGB and 32bit RGBA
--   WindowsV3, WindowsV4 and WindowsV5 formats.
-- 
--   Writing is limited to the uncompressed 24bit RGB WindowsV3 format.
--
--   We don't support the plain OS/2 BitmapCoreHeader
--       and BitmapCoreHeader2 image headers, but I haven't yet seen one of these in the wild.
-- 
-- To write a file do something like:
--
--  > do let rgba   = Data.ByteString.pack [some list of Word8s]
--  >    let bmp    = packRGBA32ToBMP width height rgba
--  >    writeBMP fileName bmp
--
-- To read a file do something like:
--
--  > do Right bmp  <- readBMP fileName
--  >    let rgba   =  unpackBMPToRGBA32 bmp
--  >    let (width, height) = bmpDimensions bmp
--  >    ... 
--      
-- Release Notes:
--
--  >  * bmp 1.2.0
--  >    Accept files with zero padding on the end of the file.
--  >    Accept RGBA files with V3 headers.
--
--  >  * bmp 1.1.2   
--  >    Accept files with the image size field set to zero.
--
module Codec.BMP
	( BMP		  (..)
	, FileHeader  	  (..)
	, BitmapInfo      (..)
	, BitmapInfoV3	  (..)
	, BitmapInfoV4    (..)
	, BitmapInfoV5    (..)
	, Compression     (..)
	, CIEXYZ          (..)
	, Error           (..)
	, readBMP
	, writeBMP
	, hGetBMP
	, hPutBMP
	, packRGBA32ToBMP 
	, unpackBMPToRGBA32
	, bmpDimensions)
where
import Codec.BMP.Base
import Codec.BMP.Error
import Codec.BMP.Unpack
import Codec.BMP.Pack
import Codec.BMP.FileHeader
import Codec.BMP.BitmapInfo
import Codec.BMP.BitmapInfoV3
import Codec.BMP.BitmapInfoV4
import Codec.BMP.BitmapInfoV5
import System.IO
import Data.ByteString		as BS
import Data.ByteString.Lazy	as BSL
import Data.Binary
import Data.Binary.Get

-- Reading ----------------------------------------------------------------------------------------
-- | Wrapper for `hGetBMP`
readBMP :: FilePath -> IO (Either Error BMP)
readBMP fileName
 = do	h	<- openBinaryFile fileName ReadMode
	hGetBMP h
	
-- | Get a BMP image from a file handle.
--	The file is checked for problems and unsupported features when read.
--	If there is anything wrong this gives an `Error` instead.
hGetBMP :: Handle -> IO (Either Error BMP)
hGetBMP h
 = do	-- lazily load the whole file
	buf	<- BSL.hGetContents h

	-- split off the file header
	let (bufFileHeader, bufRest) 
		= BSL.splitAt (fromIntegral sizeOfFileHeader) buf
	
	if (fromIntegral $ BSL.length bufFileHeader) /= sizeOfFileHeader
	 then	return $ Left ErrorFileHeaderTruncated
	 else	hGetBMP2 bufRest (decode bufFileHeader)
	
		
hGetBMP2 buf fileHeader
 -- Check the magic before doing anything else.
 | fileHeaderType fileHeader /= bmpMagic
 = return $ Left $ ErrorBadMagic (fileHeaderType fileHeader)
	
 | otherwise
 = do	-- Next comes the image header. 
	-- The first word tells us how long it is.
	let sizeHeader	= runGet getWord32le buf
	
	-- split off the image header
	let (bufImageHeader, bufRest)
		= BSL.splitAt (fromIntegral sizeHeader) buf
        
        -- How much non-header data is present in the file.
        -- For uncompressed data without a colour table, the remaining data should
        -- be the image, but there may also be padding bytes on the end.
        let physicalBufferSize
                = (fromIntegral $ BSL.length bufRest) :: Word32

	if (fromIntegral $ BSL.length bufImageHeader) /= sizeHeader
	 then 	return $ Left ErrorImageHeaderTruncated
	 else 	hGetBMP3 fileHeader bufImageHeader bufRest physicalBufferSize

			
hGetBMP3 fileHeader bufImageHeader bufRest physicalBufferSize
	| BSL.length bufImageHeader == 40 
	= do	let info	= decode bufImageHeader
		case checkBitmapInfoV3 info physicalBufferSize of
		 Just err	-> return $ Left err
		 Nothing
                  | Just imageSize      <- imageSizeFromBitmapInfoV3 info
                  -> hGetBMP4 fileHeader (InfoV3 info) bufRest imageSize
                  | otherwise
                  -> return $ Left $ ErrorInternalErrorPleaseReport

	| BSL.length bufImageHeader == 108
	= do	let info	= decode bufImageHeader
		case checkBitmapInfoV4 info physicalBufferSize of
		 Just err	-> return $ Left err
		 Nothing	
                  | Just imageSize      <- imageSizeFromBitmapInfoV4 info
                  -> hGetBMP4 fileHeader (InfoV4 info) bufRest imageSize
                  | otherwise
                  -> return $ Left $ ErrorInternalErrorPleaseReport
		
	| BSL.length bufImageHeader == 124
	= do	let info	= decode bufImageHeader
		case checkBitmapInfoV5 info physicalBufferSize of
		 Just err	-> return $ Left err
		 Nothing	
                  | Just imageSize      <- imageSizeFromBitmapInfoV5 info
                  -> hGetBMP4 fileHeader (InfoV5 info) bufRest imageSize
                  | otherwise
                  -> return $ Left $ ErrorInternalErrorPleaseReport
		
	| otherwise
 	= return $ Left 
		 $ ErrorUnhandledBitmapHeaderSize 
                 $ fromIntegral $ BSL.length bufImageHeader


hGetBMP4 fileHeader imageHeader bufImage (sizeImage :: Int)
 = let  bufLen  = fromIntegral $ BSL.length bufImage
   in   if bufLen < sizeImage
	 then return $ Left $ ErrorImageDataTruncated sizeImage bufLen
	 else return 
		$ Right $ BMP 
		{ bmpFileHeader 	= fileHeader
		, bmpBitmapInfo		= imageHeader
		, bmpRawImageData	= BS.pack $ BSL.unpack bufImage }


-- Writing ----------------------------------------------------------------------------------------
-- | Wrapper for `hPutBMP`
writeBMP :: FilePath -> BMP -> IO ()
writeBMP fileName bmp
 = do	h	<- openBinaryFile fileName WriteMode
	hPutBMP h bmp
	hFlush h
	hClose h


-- | Put a BMP image to a file handle.
hPutBMP :: Handle -> BMP -> IO ()
hPutBMP h bmp
 = do	BSL.hPut h (encode $ bmpFileHeader bmp)
	BSL.hPut h (encode $ bmpBitmapInfo bmp)
	BS.hPut h $ bmpRawImageData bmp


-- | Get the width and height of an image.
--	It's better to use this function than to access the headers directly.
bmpDimensions :: BMP -> (Int, Int)
bmpDimensions bmp	
 = let	info	= getBitmapInfoV3 $ bmpBitmapInfo bmp
   in	( fromIntegral $ dib3Width info
	, fromIntegral $ dib3Height info)


