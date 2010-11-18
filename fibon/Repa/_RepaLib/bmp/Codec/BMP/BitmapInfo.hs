{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.BitmapInfo
	( BitmapInfo	(..)
	, getBitmapInfoV3)
where
import Codec.BMP.BitmapInfoV3
import Codec.BMP.BitmapInfoV4
import Codec.BMP.BitmapInfoV5
import Data.Binary
import Data.Binary.Get

-- Image Headers ----------------------------------------------------------------------------------
-- | A wrapper for the various image header types.
--   
data BitmapInfo
	= InfoV3 BitmapInfoV3
	| InfoV4 BitmapInfoV4
	| InfoV5 BitmapInfoV5
	deriving (Show)

instance Binary BitmapInfo where
 get
  = do	size	<- lookAhead getWord32le 
	case size of
	 40 -> do
		info 	<- get
		return	$ InfoV3 info
		
	 108 -> do
		info	<- get
		return	$ InfoV4 info
		
	 124 -> do
		info	<- get
		return	$ InfoV5 info
		
	 _   -> error "Codec.BMP.BitmapInfo.get: unhandled header size"
	
 put xx
  = case xx of
	InfoV3 info	-> put info
	InfoV4 info	-> put info
	InfoV5 info	-> put info
	

-- | Get the common `BitmapInfoV3` structure from a `BitmapInfo`
getBitmapInfoV3 :: BitmapInfo -> BitmapInfoV3
getBitmapInfoV3 bi
 = case bi of
	InfoV3 info	-> info
	InfoV4 info	-> dib4InfoV3 info
	InfoV5 info	-> dib4InfoV3 $ dib5InfoV4 info
	

	
	
	
	
	
