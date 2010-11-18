module Main where

import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.BZip as BZip

main = B.interact BZip.decompress
