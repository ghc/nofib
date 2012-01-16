{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codec.BMP.Unpack
	(unpackBMPToRGBA32)
where	
import Codec.BMP.Base
import Codec.BMP.BitmapInfo
import Codec.BMP.BitmapInfoV3
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe
import Data.Word
import Data.ByteString		as BS
import Data.ByteString.Unsafe	as BS
import Prelude			as P


-- | Unpack a BMP image to a string of RGBA component values.
unpackBMPToRGBA32 :: BMP -> ByteString
unpackBMPToRGBA32 bmp 
 = let	info		= getBitmapInfoV3 $ bmpBitmapInfo bmp
	width		= fromIntegral $ dib3Width  info
	height		= fromIntegral $ dib3Height info
	bitCount	= dib3BitCount info
   in	case bitCount of
	 24	-> packRGB24ToRGBA32 width height (bmpRawImageData bmp)
	 32	-> packRGB32ToRGBA32 width height (bmpRawImageData bmp)
	 _	-> error "Codec.BMP.unpackBMPToRGBA32: unhandled bitcount."


-- | Unpack raw, uncompressed 24 bit BMP image data to a string of RGBA component values.
--	The alpha component is set to 255 for every pixel.
packRGB24ToRGBA32
	:: Int 			-- Width of image.
	-> Int			-- Height of image.
	-> ByteString 		-- Input string.
	-> ByteString
		
packRGB24ToRGBA32 width height str
 = let	bytesPerLine	= BS.length str `div` height
	padPerLine	= bytesPerLine - width * 3
	sizeDest	= width * height * 4

        -- We allow padding bytes on the end of the image data.
   in	if BS.length str < height * (width * 3 + padPerLine)
	 then error "Codec.BMP.unpackRGB24ToRGBA32: image data is truncated."
 	 else unsafePerformIO
       	 	$ allocaBytes sizeDest      $ \bufDest -> 
   	   	  BS.unsafeUseAsCString str $ \bufSrc  ->
            	   do	packRGB24ToRGBA32' width height padPerLine (castPtr bufSrc) (castPtr bufDest)
			packCStringLen (bufDest, sizeDest)

		
-- We're doing this via Ptrs because we don't want to take the
-- overhead of doing the bounds checks in ByteString.index.
packRGB24ToRGBA32' width height pad ptrSrc ptrDest 
 = go 0 0 0 0
 where	
	go posX posY oSrc oDest
	 -- skip over padding bytes at the end of each line.
	 | posX == width 
	 = go 0 (posY + 1) (oSrc + pad) oDest
	
	 -- we've finished the image.
	 | posY == height
	 = return ()
	
	 -- process a pixel.
	 | otherwise
	 = do	blue  :: Word8	<- peekByteOff ptrSrc (oSrc + 0)
		green :: Word8	<- peekByteOff ptrSrc (oSrc + 1)
		red   :: Word8	<- peekByteOff ptrSrc (oSrc + 2)

		pokeByteOff ptrDest (oDest + 0) red
		pokeByteOff ptrDest (oDest + 1) green
		pokeByteOff ptrDest (oDest + 2) blue
		pokeByteOff ptrDest (oDest + 3) (255 :: Word8)
		
		go (posX + 1) posY (oSrc + 3) (oDest + 4)



-- | Unpack raw, uncompressed 32 bit BMP image data to a string of RGBA component values.
--   Note in the BMP file the components are arse-around ABGR instead of RGBA. 
--   The 'unpacking' here is really just flipping the components around.
packRGB32ToRGBA32
	:: Int 			-- Width of image.
	-> Int			-- Height of image.
	-> ByteString 		-- Input string.
	-> ByteString
		
packRGB32ToRGBA32 width height str
  = let sizeDest = height * width * 4
    in  if  BS.length str < sizeDest
	 then error "Codec.BMP.packRGB24ToRGBA32: image data is truncated."
 	 else unsafePerformIO
       	 	$ allocaBytes sizeDest      $ \bufDest -> 
   	   	  BS.unsafeUseAsCString str $ \bufSrc  ->
            	   do	packRGB32ToRGBA32' width height (castPtr bufSrc) (castPtr bufDest)
			packCStringLen (bufDest, sizeDest)
		
-- We're doing this via Ptrs because we don't want to take the
-- overhead of doing the bounds checks in ByteString.index.
packRGB32ToRGBA32' width height ptrSrc ptrDest 
 = go 0 0 0 0
 where	
	go posX posY oSrc oDest
	 -- skip over padding bytes at the end of each line.
	 | posX == width 
	 = go 0 (posY + 1) oSrc oDest
	
	 -- we've finished the image.
	 | posY == height
	 = return ()
	
	 -- process a pixel.
	 | otherwise
	 = do	blue   :: Word8	<- peekByteOff ptrSrc (oSrc + 0)
		green  :: Word8	<- peekByteOff ptrSrc (oSrc + 1)
		red    :: Word8	<- peekByteOff ptrSrc (oSrc + 2)
		alpha  :: Word8 <- peekByteOff ptrSrc (oSrc + 3)

		pokeByteOff ptrDest (oDest + 0) red
		pokeByteOff ptrDest (oDest + 1) green
		pokeByteOff ptrDest (oDest + 2) blue
		pokeByteOff ptrDest (oDest + 3) alpha
		
		go (posX + 1) posY (oSrc + 4) (oDest + 4)


		
