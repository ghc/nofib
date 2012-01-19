{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.Compression
        (Compression(..))
where
import Data.Word
import Data.Binary
import Data.Binary.Get  
import Data.Binary.Put


data Compression
        = CompressionRGB
        | CompressionRLE8
        | CompressionRLE4
        | CompressionBitFields
        | CompressionJPEG
        | CompressionPNG
        | CompressionUnknown Word32
        deriving (Show, Eq)


instance Binary Compression where
 get
  = do  c       <- getWord32le
        case c of
         0      -> return $ CompressionRGB
         1      -> return $ CompressionRLE8
         2      -> return $ CompressionRLE4
         3      -> return $ CompressionBitFields
         4      -> return $ CompressionJPEG
         5      -> return $ CompressionPNG
         _      -> return $ CompressionUnknown c
        
 put c
  = case c of
        CompressionRGB          -> putWord32le 0
        CompressionRLE8         -> putWord32le 1
        CompressionRLE4         -> putWord32le 2
        CompressionBitFields    -> putWord32le 3
        CompressionJPEG         -> putWord32le 4
        CompressionPNG          -> putWord32le 5
        CompressionUnknown x    -> putWord32le x
        
