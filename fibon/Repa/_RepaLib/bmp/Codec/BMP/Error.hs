{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.Error
	(Error(..))
where
import Codec.BMP.Compression
import Data.Word

-- | Things that can go wrong when loading a BMP file.
data Error

        -- | Magic number was not at the start of the file, 
        --   so this probably isn't a BMP file.
        = ErrorBadMagic                         Word16

        -- | File is too short to contain a file header.
	| ErrorFileHeaderTruncated

        -- | File is too short to contain an image header.
	| ErrorImageHeaderTruncated

        -- | File is too short to contain the image data.
	| ErrorImageDataTruncated
        { errorBytesNeeded      :: Int
        , errorBytesAvailable   :: Int }

        -- | Reserved fields should be zero.
	| ErrorReservedFieldNotZero

        -- | The offset to the image data from the file header doesn't
        --   point anywhere sensible.
	| ErrorDodgyFileHeaderFieldOffset
        { errorFileHeaderOffset :: Word32 }

        -- | We handle V3 V4 and V5 image headers, but the size of 
        --   the header indicates it has some other format.
	| ErrorUnhandledBitmapHeaderSize
        { errorBitmapHeaderSize :: Word32 }

        -- | We only handle single color planes.
	| ErrorUnhandledPlanesCount
        { errorPlanesCount      :: Word16 }

        -- | We only handle 24 and 32 bit images.
	| ErrorUnhandledColorDepth
        { errorColorDepth       :: Word16 }

        -- | We only handle uncompressed images.
	| ErrorUnhandledCompressionMode
        { errorCompression      :: Compression}

        -- | Mismatch between the image size stated in the header
        --   and that which is calculuated from the other fields.
        | ErrorImagePhysicalSizeMismatch 
        { errorImageSizeFromHeader  :: Word32
        , errorImageSizeOfBuffer    :: Word32 }

        -- | Something went wrong in the library.
        | ErrorInternalErrorPleaseReport
	deriving (Eq, Show)

