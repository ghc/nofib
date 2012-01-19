
module Codec.BMP.CIEXYZ
	(CIEXYZ(..))
where
import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put 

-- | Contains the XYZ coordinates of a specific color in a specified color space.
data CIEXYZ 
	= CIEXYZ Word32 Word32 Word32
	deriving Show


instance Binary CIEXYZ where
 get 
  = do	r	<- getWord32le
	g	<- getWord32le
	b	<- getWord32le
	return	$ CIEXYZ r g b
	
 put (CIEXYZ r g b)
  = do	putWord32le r
	putWord32le g
	putWord32le b

