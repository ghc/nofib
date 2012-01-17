module Codec.Binary.BubbleBabble(encode) where


import Data.Array.Unboxed
import Data.Bits
import Data.Word

import Codec.Utils


vowel :: UArray Int Char
vowel = listArray (0,5) "aeiouy"

consonant :: UArray Int Char
consonant = listArray (0,16) "bcdfghklmnprstvzx"


-- | Encode binary data into the bubble babble human readable encoding.
-- Bubble Babble is an encoding that represents binary data as psuedowords
-- which are more pronouncable and memorable than standard hexadecimal encoding.
--
-- It is mainly used for representing cryptographic fingerprints.
-- In addition, there is an amount of redundancy and error correction built into
-- the representation so that transcription errors can be more readily identified.
--
-- see:  http://en.wikipedia.org/wiki/Bubble_Babble
--

encode :: [Octet] -> String
encode cs = 'x' : bb 1 (map fromIntegral cs) where
    bb seed [] = vcvx  ((seed `mod` 6),16,(seed `div` 6))
    bb seed [x] = vcvx ((((x `shiftR` 6) .&. 3) + seed) `mod` 6, (x `shiftR` 2) .&. 15, ((x .&. 3) + (seed `div` 6)) `mod` 6)
    bb seed (x:y:xs) = vcvcc (a,b,c,d,e) $ bb ((seed * 5 + (x * 7 + y)) `mod` 36) xs where
        a = (((x `shiftR` 6) .&. 3) + seed) `mod` 6
        b = (x `shiftR` 2) .&. 15
        c = ((x .&. 3) + (seed `div` 6)) `mod` 6
        d = (y `shiftR` 4) .&. 15
        e = y .&. 15
    vcvx (a,b,c) = vowel!a : consonant!b : vowel!c : "x"
    vcvcc (a,b,c,d,e) xs =  vowel!a : consonant!b : vowel!c : consonant!d : '-' : consonant!e : xs

