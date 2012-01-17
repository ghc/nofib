{-# OPTIONS_GHC -funbox-strict-fields #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Digest.SHA2
-- Copyright   :  (c) Russell O'Connor 2006
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Implements SHA-256, SHA-384, SHA-512, and SHA-224 as defined in FIPS 180-2
-- <http://csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf>.
--
-----------------------------------------------------------------------------

module Data.Digest.SHA2 (sha256, sha256Ascii, Hash256
                        ,sha512, sha512Ascii, Hash512
                        ,sha384, sha384Ascii, Hash384
                        ,sha224, sha224Ascii, Hash224
                        ,toOctets) where

import Data.Word
import Data.Bits
import Data.List
import Numeric
import Test.HUnit

ch x y z = (x .&. y) `xor` (complement x .&. z)
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

class (Bits w) => ShaData w where
  bigSigma0 :: w -> w
  bigSigma1 :: w -> w
  smallSigma0 :: w -> w
  smallSigma1 :: w -> w
  ks :: [w]

instance ShaData Word32 where
 bigSigma0 x = rotateR x 2 `xor` rotateR x 13 `xor` rotateR x 22
 bigSigma1 x = rotateR x 6 `xor` rotateR x 11 `xor` rotateR x 25
 smallSigma0 x = rotateR x 7 `xor` rotateR x 18 `xor` shiftR x 3
 smallSigma1 x = rotateR x 17 `xor` rotateR x 19 `xor` shiftR x 10
 ks = 
   [0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
   ,0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
   ,0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
   ,0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
   ,0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
   ,0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
   ,0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
   ,0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]

instance ShaData Word64 where
 bigSigma0 x = rotateR x 28 `xor` rotateR x 34 `xor` rotateR x 39
 bigSigma1 x = rotateR x 14 `xor` rotateR x 18 `xor` rotateR x 41
 smallSigma0 x = rotateR x 1 `xor` rotateR x 8 `xor` shiftR x 7
 smallSigma1 x = rotateR x 19 `xor` rotateR x 61 `xor` shiftR x 6
 ks = 
   [0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc
   ,0x3956c25bf348b538, 0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118
   ,0xd807aa98a3030242, 0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2
   ,0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 0xc19bf174cf692694
   ,0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65
   ,0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5
   ,0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4
   ,0xc6e00bf33da88fc2, 0xd5a79147930aa725, 0x06ca6351e003826f, 0x142929670a0e6e70
   ,0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df
   ,0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b
   ,0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30
   ,0xd192e819d6ef5218, 0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8
   ,0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8
   ,0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3
   ,0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec
   ,0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b
   ,0xca273eceea26619c, 0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178
   ,0x06f067aa72176fba, 0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b
   ,0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c
   ,0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817]

blockSize = 16

-----------------------------------------------------------------------------
-- | 'padding' currently requires that the bitSize of @a@ divide the bitSize 
-- of @w@
-----------------------------------------------------------------------------
padding :: (ShaData w, Bits a, Integral a) => [a] -> [[w]]
padding x = unfoldr block $ paddingHelper x 0 (0::Int) (0::Integer)
 where
  block [] = Nothing
  block x = Just $ splitAt blockSize x
  paddingHelper x o on n | on == (bitSize o) = o:paddingHelper x 0 0 n
  paddingHelper (x:xs) o on n | on < (bitSize o) =
    paddingHelper xs ((shiftL o bs) .|. (fromIntegral x)) (on+bs) $! (n+fromIntegral bs)
   where
    bs = bitSize x
  paddingHelper [] o on n = (shiftL (shiftL o 1 .|. 1) (bso-on-1)):
                            (zeros ((-(fromIntegral n-on+3*bso)) `mod` (blockSize*bso)))
                            [fromIntegral (shiftR n bso), fromIntegral n]
   where
    bso = bitSize o
    zeros 0 = id
    zeros n | 0 < n = let z=0 in (z:) . (zeros (n-bitSize z))

data Hash8 w = Hash8 !w !w !w !w !w !w !w !w deriving (Eq, Ord)

type Hash256 = Hash8 Word32
type Hash512 = Hash8 Word64

data Hash384 = Hash384 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 deriving (Eq, Ord)
data Hash224 = Hash224 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 deriving (Eq, Ord)

instance (Integral a) => Show (Hash8 a) where
 showsPrec _ (Hash8 a b c d e f g h) =
  (showHex a) . (' ':) .
  (showHex b) . (' ':) .
  (showHex c) . (' ':) .
  (showHex d) . (' ':) .
  (showHex e) . (' ':) .
  (showHex f) . (' ':) .
  (showHex g) . (' ':) .
  (showHex h)

instance Show Hash384 where
 showsPrec _ (Hash384 a b c d e f) =
  (showHex a) . (' ':) .
  (showHex b) . (' ':) .
  (showHex c) . (' ':) .
  (showHex d) . (' ':) .
  (showHex e) . (' ':) .
  (showHex f)

instance Show Hash224 where
 showsPrec _ (Hash224 a b c d e f g) =
  (showHex a) . (' ':) .
  (showHex b) . (' ':) .
  (showHex c) . (' ':) .
  (showHex d) . (' ':) .
  (showHex e) . (' ':) .
  (showHex f) . (' ':) .
  (showHex g)

class (Eq h, Ord h, Show h) => Hash h where
  toOctets :: h -> [Word8]

bitsToOctets x = helper (bitSize x) x []
   where
    helper s x r | s <= 0 = r
                 | otherwise = helper (s-bs) (shiftR x bs) ((fromIntegral x):r)
     where
      bs = bitSize (head r)

instance (Integral h, Bits h) => Hash (Hash8 h) where
  toOctets (Hash8 x0 x1 x2 x3 x4 x5 x6 x7) = bitsToOctets =<< [x0, x1, x2, x3, x4, x5, x6, x7]

instance Hash Hash384 where
  toOctets (Hash384 x0 x1 x2 x3 x4 x5) = bitsToOctets =<< [x0, x1, x2, x3, x4, x5]

instance Hash Hash224 where
  toOctets (Hash224 x0 x1 x2 x3 x4 x5 x6) = bitsToOctets =<< [x0, x1, x2, x3, x4, x5, x6]

shaStep :: (ShaData w) => Hash8 w -> [w] -> Hash8 w
shaStep h m = (foldl' (flip id) h (zipWith mkStep3 ks ws)) `plus` h
 where
  ws = m++zipWith4 smallSigma (drop (blockSize-2) ws) (drop (blockSize-7) ws)
                              (drop (blockSize-15) ws) (drop (blockSize-16) ws)
   where
    smallSigma a b c d = smallSigma1 a + b + smallSigma0 c + d
  mkStep3 k w (Hash8 a b c d e f g h) = Hash8 (t1+t2) a b c (d+t1) e f g
   where
    t1 = h + bigSigma1 e + ch e f g + k + w
    t2 = bigSigma0 a + maj a b c
  (Hash8 x0 x1 x2 x3 x4 x5 x6 x7) `plus` (Hash8 y0 y1 y2 y3 y4 y5 y6 y7) =
    Hash8 (x0+y0) (x1+y1) (x2+y2) (x3+y3) (x4+y4) (x5+y5) (x6+y6) (x7+y7)

-----------------------------------------------------------------------------
-- | Due to the limitations of 'padding', 'sha' currently requires that the
-- bitSize of @a@ divide the bitSize of @w@
-----------------------------------------------------------------------------
sha :: (ShaData w, Bits a, Integral a) => Hash8 w -> [a] -> Hash8 w
sha h0 x = foldl' shaStep h0 $ padding x

stringToOctets :: String -> [Word8]
stringToOctets = map (fromIntegral . fromEnum)

-----------------------------------------------------------------------------
-- | 'sha256' currently requires that the bitSize of @a@ divide 32
-----------------------------------------------------------------------------
sha256 :: (Bits a, Integral a) => [a] -> Hash256
sha256 = sha $
  Hash8 0x6a09e667 0xbb67ae85 0x3c6ef372 0xa54ff53a 0x510e527f 0x9b05688c 0x1f83d9ab 0x5be0cd19

-----------------------------------------------------------------------------
-- | 'sha384' currently requires that the bitSize of @a@ divide 64
-----------------------------------------------------------------------------
sha384 :: (Bits a, Integral a) => [a] -> Hash384
sha384 x = Hash384 x0 x1 x2 x3 x4 x5
 where
  Hash8 x0 x1 x2 x3 x4 x5 x6 x7 = flip sha x $
    Hash8 0xcbbb9d5dc1059ed8 0x629a292a367cd507 0x9159015a3070dd17 0x152fecd8f70e5939
          0x67332667ffc00b31 0x8eb44a8768581511 0xdb0c2e0d64f98fa7 0x47b5481dbefa4fa4

-----------------------------------------------------------------------------
-- | 'sha384' currently requires that the bitSize of @a@ divide 64
-----------------------------------------------------------------------------
sha512 :: (Bits a, Integral a) => [a] -> Hash512
sha512 = sha $
  Hash8 0x6a09e667f3bcc908 0xbb67ae8584caa73b 0x3c6ef372fe94f82b 0xa54ff53a5f1d36f1
        0x510e527fade682d1 0x9b05688c2b3e6c1f 0x1f83d9abfb41bd6b 0x5be0cd19137e2179

-----------------------------------------------------------------------------
-- | 'sha224' currently requires that the bitSize of @a@ divide 32
-----------------------------------------------------------------------------
sha224 :: (Bits a, Integral a) => [a] -> Hash224
sha224 x = Hash224 x0 x1 x2 x3 x4 x5 x6
 where
  Hash8 x0 x1 x2 x3 x4 x5 x6 x7 = flip sha x $
    Hash8 0xc1059ed8 0x367cd507 0x3070dd17 0xf70e5939 0xffc00b31 0x68581511 0x64f98fa7 0xbefa4fa4

-----------------------------------------------------------------------------
-- ** Hashing Strings
-- | @shaXXXAscii@ assumes that all characters of the strings are 
-- ISO-latin-1 characters.  ie. each characters fits in one octet.
-----------------------------------------------------------------------------
sha256Ascii :: String -> Hash256
sha256Ascii = sha256 . stringToOctets

sha384Ascii :: String -> Hash384
sha384Ascii = sha384 . stringToOctets

sha512Ascii :: String -> Hash512
sha512Ascii = sha512 . stringToOctets

sha224Ascii :: String -> Hash224
sha224Ascii = sha224 . stringToOctets

-----------------------------------------------------------------------------
-- ** Test cases
-- | Below are test cases from the FIPS 180-2 document
-----------------------------------------------------------------------------

-- Should this go into it's own module?

test_sha256 = "SHA-256" ~: test
              [sha256Ascii "abc" ~?=
               Hash8 0xba7816bf 0x8f01cfea 0x414140de 0x5dae2223 0xb00361a3 0x96177a9c 0xb410ff61 0xf20015ad
              ,sha256Ascii "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" ~?=
               Hash8 0x248d6a61 0xd20638b8 0xe5c02693 0x0c3e6039 0xa33ce459 0x64ff2167 0xf6ecedd4 0x19db06c1
              ,sha256Ascii (replicate 1000000 'a') ~?=
               Hash8 0xcdc76e5c 0x9914fb92 0x81a1c7e2 0x84d73e67 0xf1809a48 0xa497200e 0x046d39cc 0xc7112cd0]

test_sha512 = "SHA-512" ~: test
              [sha512Ascii "abc" ~?=
               Hash8 0xddaf35a193617aba 0xcc417349ae204131 0x12e6fa4e89a97ea2 0x0a9eeee64b55d39a
                     0x2192992a274fc1a8 0x36ba3c23a3feebbd 0x454d4423643ce80e 0x2a9ac94fa54ca49f
              ,sha512Ascii ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn"++
                            "hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu") ~?=
               Hash8 0x8e959b75dae313da 0x8cf4f72814fc143f 0x8f7779c6eb9f7fa1 0x7299aeadb6889018
                     0x501d289e4900f7e4 0x331b99dec4b5433a 0xc7d329eeb6dd2654 0x5e96e55b874be909
              ,sha512Ascii (replicate 1000000 'a') ~?=
               Hash8 0xe718483d0ce76964 0x4e2e42c7bc15b463 0x8e1f98b13b204428 0x5632a803afa973eb
                     0xde0ff244877ea60a 0x4cb0432ce577c31b 0xeb009c5c2c49aa2e 0x4eadb217ad8cc09b]

test_sha384 = "SHA-384" ~: test
              [sha384Ascii "abc" ~?=
               Hash384 0xcb00753f45a35e8b 0xb5a03d699ac65007 0x272c32ab0eded163 0x1a8b605a43ff5bed
                       0x8086072ba1e7cc23 0x58baeca134c825a7
              ,sha384Ascii ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn"++
                            "hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu") ~?=
               Hash384 0x09330c33f71147e8 0x3d192fc782cd1b47 0x53111b173b3b05d2 0x2fa08086e3b0f712
                       0xfcc7c71a557e2db9 0x66c3e9fa91746039
              ,sha384Ascii (replicate 1000000 'a') ~?=
               Hash384 0x9d0e1809716474cb 0x086e834e310a4a1c 0xed149e9c00f24852 0x7972cec5704c2a5b
                       0x07b8b3dc38ecc4eb 0xae97ddd87f3d8985]

test_sha224 = "SHA-224" ~: test 
              [sha224Ascii "abc" ~?=
               Hash224 0x23097d22 0x3405d822 0x8642a477 0xbda255b3 0x2aadbce4 0xbda0b3f7 0xe36c9da7
              ,sha224Ascii "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" ~?=
               Hash224 0x75388b16 0x512776cc 0x5dba5da1 0xfd890150 0xb0c6455c 0xb4f58b19 0x52522525
              ,sha224Ascii (replicate 1000000 'a') ~?=
               Hash224 0x20794655 0x980c91d8 0xbbb4c1ea 0x97618a4b 0xf03f4258 0x1948b2ee 0x4ee7ad67]

test_sha2 = "SHA-2" ~: test
            [test_sha256, test_sha512, test_sha384, test_sha224]

-- Test with:
-- ghc -no-recomp -O --make Data/Digest/SHA2.hs -main-is Data.Digest.SHA2.moduleTest -o moduleTest && ./moduleTest && rm moduleTest
moduleTest = runTestTT test_sha2