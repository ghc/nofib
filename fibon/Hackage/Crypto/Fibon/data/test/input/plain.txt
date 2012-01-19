module Main(main) where

import Control.Monad
import Control.Applicative
import Codec.Binary.BubbleBabble as Bubble
import Codec.Utils
import Codec.Encryption.Blowfish as Blowfish
import Codec.Encryption.Modes
import Codec.Encryption.Padding
import Codec.Encryption.DES as DES
import Codec.Encryption.AES as AES
import Codec.Encryption.RSA as RSA
import Codec.Encryption.RSA.EMEOAEP as EMEOAEP
import Codec.Encryption.RSA.MGF
import Data.Digest.MD5  as MD5
import Data.Digest.SHA1 as SHA1
import Data.Word
import Data.Bits
import Data.Char
import Data.LargeWord
import Numeric

import qualified Data.ByteString as B
import System.Environment


main = do
  fs <- getArgs
  mapM_ crypto fs

crypto :: FilePath -> IO ()
crypto f = do
  putStrLn f
  d <- B.unpack `liftM` B.readFile f
  sequence_ $ algs <*> [d]
  where
    algs = [aes, des, blowfish]

aes :: [Word8] -> IO ()
aes = crypt listFromOctets AES.encrypt ak32

blowfish :: [Word8] -> IO ()
blowfish = crypt listFromOctets Blowfish.encrypt bk16

des :: [Word8] -> IO ()
des = crypt listFromOctets DES.encrypt dk8

crypt :: (Bits a, Integral a, Integral b) => 
         ([Word8] -> [a]) -- conversion from Word8 to BlockSize
      -> (b -> a -> a)    -- encryption function
      -> b                -- encryption key
      -> [Word8]          -- input
      -> IO ()            -- print out the result
crypt p e k d = output cypherText
  where
  d' = p d
  cypherText = map (e k) d'

output :: (Bits a, Integral a) => [a] -> IO ()
output = putStrLn . Bubble.encode . MD5.hash . listToOctets


-- Encryption Keys
bk16 :: Word128
bk16 = 0xF0E1D2C3B4A5968778695A4B3C2D1E0F

ak32 :: Word256
ak32 = 0xF0E1D2C3B4A5968778695A4B3C2D1E0FF0E1D2C3B4A5968778695A4B3C2D1E0F

dk8  :: Word64
dk8  = 0x0123456789abcdef

