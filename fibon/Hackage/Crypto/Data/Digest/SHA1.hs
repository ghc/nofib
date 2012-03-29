{-# OPTIONS_GHC -funbox-strict-fields #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Digest.SHA1
-- Copyright   :  (c) Dominic Steinitz 2007
-- License     :  BSD-style (see the file ReadMe.tex)
--
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Take [Word8] and return Word160.
-- See <http://www.itl.nist.gov/fipspubs/fip180-1.htm> for the specification.
--
-----------------------------------------------------------------------------

module Data.Digest.SHA1(
   Word160(Word160),
   hash,
   lift2,
   toInteger
   ) where

import Data.Bits
import Data.List
import Data.Word
import Data.Array.IArray
import Codec.Utils
import Prelude hiding (toInteger)

rotL :: Bits b => Int -> b -> b
rotL = flip rotateL

data Word160 = Word160 {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
               deriving (Eq, Show)

toInteger :: Word160 -> Integer
toInteger (Word160 a b c d e) = let n = fromIntegral e +
                                        (fromIntegral d `shiftL` 32) +
                                        (fromIntegral c `shiftL` 64) +
                                        (fromIntegral b `shiftL` 96) +
                                        (fromIntegral a `shiftL` 128)
                                in n `seq` n

lift2 :: (Word32 -> Word32 -> Word32) -> Word160 -> Word160 -> Word160
lift2 f a@(Word160 x1 x2 x3 x4 x5) b@(Word160 y1 y2 y3 y4 y5) = 
   Word160 (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4) (f x5 y5)

f n x y z 
   | n <= 19 = (x .&. y) .|. ((complement x) .&. z)
   | n <= 39 = x `xor` y `xor` z
   | n <= 59 = (x .&. y) .|. (x .&. z) .|. (y .&. z)
   | n <= 79 = x `xor` y `xor` z

k n
   | n <= 19 = 0x5a827999
   | n <= 39 = 0x6ed9eba1
   | n <= 59 = 0x8f1bbcdc
   | n <= 79 = 0xca62c1d6

data AccAndWord160 = AccAndWord160 !Int !Word160

-- Word160 -> Word512 -> Word160 
oneBlock ss xs = tt
   where
      us :: Array Int Word32
      us = 
         accumArray (curry snd) 0 (0,79) (zip [0..15] xs ++ map (\(x,y) -> (x, rotL 1 y))[(i, xxor i) | i<-[16..79]])
            where
               xxor i = us ! (i-16) `xor` us ! (i-3) `xor` us ! (i-8) `xor` us ! (i-14)
      g (AccAndWord160 n (Word160 a b c d e)) w = 
         AccAndWord160 (n+1) (Word160 ((rotL 5 a) + (f n b c d) + e + w + (k n)) a (rotL 30 b) c d)
      (AccAndWord160 _ tt) = foldl' g (AccAndWord160 0 ss) (elems us)

ss :: Word160
ss = Word160 0x67452301 0xefcdab89 0x98badcfe 0x10325476 0xc3d2e1f0

pad = pad' 0
   where pad' l [] = [0x80] ++ ps ++ lb
          where pl = (64-(l+9)) `mod` 64
                ps = replicate pl 0x00
                lb = i2osp 8 (8*l)
         pad' l (x:xs) = x : (pad' $! l+1) xs -- otherwise (l+1) it will be deferred until replicate

blockWord8sIn512 :: [Word8] -> [[Word8]]
blockWord8sIn512 =
   unfoldr g
   where
      g [] = Nothing
      g xs = Just (splitAt 64 xs)

fromBytes :: (Num a, Bits a) => [a] -> a
fromBytes input =
    let dofb accum [] = accum
        dofb accum (x:xs) = dofb ((shiftL accum 8) .|. x) xs
        in
        dofb 0 input

blockWord8sIn32 :: [Word8] -> [[Word8]]
blockWord8sIn32 =
   unfoldr g
   where
      g [] = Nothing
      g xs = Just (splitAt 4 xs)

getWord32s :: [Word8] -> [Word32]
getWord32s =
   map fromBytes . map (map fromIntegral) .  blockWord8sIn32

blockWord32sIn512 :: [Word8] -> [[Word32]]
blockWord32sIn512 = (map getWord32s) . blockWord8sIn512 . pad

hashOnce ss a = lift2 (+) ss (oneBlock ss a)

hash :: [Word8] -> Word160
hash = foldl' hashOnce ss . blockWord32sIn512
