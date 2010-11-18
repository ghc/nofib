-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HMAC
-- Copyright   :  (c) Greg Heartsfield 2007
-- License     :  BSD-style (see the file ReadMe.tex)
--
-- Implements HMAC (hashed message authentication code) as defined in FIPS 198
-- <http://csrc.nist.gov/publications/fips/fips198/fips-198a.pdf>.
--
-----------------------------------------------------------------------------

module Data.HMAC(
   -- * Function Types
   hmac, hmac_sha1, hmac_md5,
   -- * Data Types
   HashMethod(HashMethod, digest, input_blocksize),
   ) where

import Data.Digest.SHA1 as SHA1
import Data.Digest.MD5 as MD5
import Data.Word (Word32)
import Data.Bits (shiftR, xor, bitSize, Bits)
import Codec.Utils (Octet)

-- | HMAC works over any hash function, which is represented by
--   HashMethod.  A hash function and input block size must
--   be specified.

data HashMethod =
    HashMethod { -- | An arbitrary hash function
                 digest :: [Octet] -> [Octet],
                -- | Bit size of an input block to the hash function
                 input_blocksize :: Int}

-- Some useful digest functions for use with HMAC.

sha1_hm = HashMethod (w160_to_w8s . SHA1.hash) 512
md5_hm = HashMethod MD5.hash 512

-- | Compute an HMAC using SHA-1 as the underlying hash function.

hmac_sha1 :: [Octet] -- ^ Secret key
          -> [Octet] -- ^ Message text
          -> [Octet] -- ^ Resulting HMAC-SHA1 value
hmac_sha1 = hmac sha1_hm

-- | Compute an HMAC using MD5 as the underlying hash function.

hmac_md5 :: [Octet] -- ^ Secret key
         -> [Octet] -- ^ Message text
         -> [Octet] -- ^ Resulting HMAC-MD5 value
hmac_md5 = hmac md5_hm

w160_to_w8s :: Word160 -> [Octet]
w160_to_w8s w = concat $ map w32_to_w8s (w160_to_w32s w)

w160_to_w32s :: Word160 -> [Word32]
w160_to_w32s (Word160 a b c d e) = a : b : c : d : e : []

w32_to_w8s :: Word32 -> [Octet]
w32_to_w8s a = (fromIntegral (shiftR a 24)) :
               (fromIntegral (shiftR a 16)) :
               (fromIntegral (shiftR a 8)) :
               (fromIntegral a) : []

-- | Generalized function for creating HMACs on a specified
--   hash function.

hmac :: HashMethod -- ^ Hash function and associated block size
        -> [Octet] -- ^ Secret key
        -> [Octet] -- ^ Message text
        -> [Octet] -- ^ Resulting HMAC value
hmac h uk m = hash (opad ++ (hash (ipad ++ m)))
    where hash = digest h
          (opad, ipad) = process_pads key
                           (make_start_pad bs opad_pattern)
                           (make_start_pad bs ipad_pattern)
          bs = input_blocksize h
          key = key_from_user h uk

-- Create a key of the proper size from the user-supplied key.
-- Keys greater than blocksize get hashed and padded with zeroes.
-- Keys same as blocksize are used as is.
-- Keys shorter than blocksize are padding with zeroes.

key_from_user :: HashMethod -> [Octet] -> [Octet]
key_from_user h uk =
    case (compare (bitcount uk) (input_blocksize h)) of
      GT -> fill_key ((digest h) uk)
      LT -> fill_key uk
      EQ -> uk
    where fill_key kd =
              kd ++ (take (((input_blocksize h) - (bitcount kd)) `div` 8)
                     (repeat 0x0))

-- Create the inner/outer pad values by XOR'ing with the key.

process_pads :: [Octet] -- Key
             -> [Octet] -- opad
             -> [Octet] -- ipad
             -> ([Octet], [Octet]) -- new opad, new ipad
process_pads ks os is =
    unzip $ zipWith3 (\k o i -> (k `xor` o, k `xor` i)) ks os is

-- Create padding values for a hash of a given bit size.

make_start_pad :: Int -> Octet -> [Octet]
make_start_pad size pad = take (size `div` (bitSize pad)) $ repeat pad

-- Padding constants, per the spec.

opad_pattern = 0x5c :: Octet
ipad_pattern = 0x36 :: Octet

-- Bit count of byte array.

bitcount :: [Octet] -> Int
bitcount k = (length k) * (bitSize (head k))
