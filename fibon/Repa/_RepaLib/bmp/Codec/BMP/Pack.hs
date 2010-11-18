{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codec.BMP.Pack
	(packRGBA32ToBMP)
where
import Codec.BMP.Base
import Codec.BMP.BitmapInfo
import Codec.BMP.BitmapInfoV3
import Codec.BMP.FileHeader
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe
import Data.Word
import Data.Maybe
import Data.ByteString		as BS
import Data.ByteString.Unsafe	as BS
import Prelude			as P

-- | Pack a string of RGBA component values into a BMP image.
--	If the given dimensions don't match the input string then `error`.
--	This currently ignores the alpha component of the input string and produces a 24bit RGB image.
packRGBA32ToBMP
	:: Int 		-- ^ Width of image.
	-> Int 		-- ^ Height of image.
	-> ByteString	-- ^ A string of RGBA component values. Must have length (@width * height * 4@)
	-> BMP
	
packRGBA32ToBMP width height str
 | height * width * 4 /= BS.length str
 = error "Codec.BMP.packRGBAToBMP: given image dimensions don't match input data."

 | otherwise
 = let	(imageData, _)	= packRGBA32ToRGB24 width height str

	fileHeader
		= FileHeader
		{ fileHeaderType	= bmpMagic

		, fileHeaderFileSize	= fromIntegral
					$ sizeOfFileHeader + sizeOfBitmapInfoV3	+ BS.length imageData

		, fileHeaderReserved1	= 0
		, fileHeaderReserved2	= 0
		, fileHeaderOffset	= fromIntegral (sizeOfFileHeader + sizeOfBitmapInfoV3) }

	bitmapInfoV3
		= BitmapInfoV3
		{ dib3Size		= fromIntegral sizeOfBitmapInfoV3
		, dib3Width		= fromIntegral width
		, dib3Height		= fromIntegral height
		, dib3Planes		= 1
		, dib3BitCount		= 24
		, dib3Compression	= CompressionRGB
		, dib3ImageSize		= fromIntegral $ BS.length imageData

		-- The default resolution seems to be 72 pixels per inch.
		--	This equates to 2834 pixels per meter.
		--	Dunno WTF this should be in the header though...
		, dib3PelsPerMeterX	= 2834
		, dib3PelsPerMeterY	= 2834

		, dib3ColorsUsed	= 0
		, dib3ColorsImportant	= 0 }
		
	errs	= catMaybes		
			[ checkFileHeader   fileHeader
			, checkBitmapInfoV3 bitmapInfoV3 ]
		
   in	case errs of
	 [] -> BMP 
		{ bmpFileHeader		= fileHeader
		, bmpBitmapInfo		= InfoV3 bitmapInfoV3
		, bmpRawImageData	= imageData }
	 
	 _  -> error $ "Codec.BMP: packRGBA32ToBMP constructed BMP file has errors, sorry.\n" ++ show errs



packRGBA32ToRGB24 
	:: Int			-- ^ Width of image.
	-> Int			-- ^ Height of image.
	-> ByteString
	-> (ByteString, Int)	-- output bytestring, and number of pad bytes per line.
	
packRGBA32ToRGB24 width height str
 | height * width * 4 /= BS.length str
 = error "Codec.BMP.packRGBAToRGB24: given image dimensions don't match input data."

 | otherwise
 = let	padPerLine	
	 = case (width * 3) `mod` 4 of
		0	-> 0
		x	-> 4 - x
				
	sizeDest	= height * (width * 3 + padPerLine)
   in	unsafePerformIO
	 $ allocaBytes sizeDest 	$ \bufDest ->
	   BS.unsafeUseAsCString str	$ \bufSrc  ->
	    do	packRGBA32ToRGB24' width height padPerLine (castPtr bufSrc) (castPtr bufDest)
		bs	<- packCStringLen (bufDest, sizeDest)
		return	(bs, padPerLine)
	
			
packRGBA32ToRGB24' width height pad ptrSrc ptrDest
 = go 0 0 0 0
 where
	go posX posY oSrc oDest

	 -- add padding bytes at the end of each line.
	 | posX == width
	 = do	mapM_ (\n -> pokeByteOff ptrDest (oDest + n) (0 :: Word8)) 
			$ P.take pad [0 .. ]
		go 0 (posY + 1) oSrc (oDest + pad)
		
	 -- we've finished the image.
	 | posY == height
	 = return ()
	
	 -- process a pixel
	 | otherwise
	 = do	
		red	:: Word8  <- peekByteOff ptrSrc (oSrc + 0)
		green	:: Word8  <- peekByteOff ptrSrc (oSrc + 1)
		blue	:: Word8  <- peekByteOff ptrSrc (oSrc + 2)
	
		pokeByteOff ptrDest (oDest + 0) blue
		pokeByteOff ptrDest (oDest + 1) green
		pokeByteOff ptrDest (oDest + 2) red
		
		go (posX + 1) posY (oSrc + 4) (oDest + 3)


