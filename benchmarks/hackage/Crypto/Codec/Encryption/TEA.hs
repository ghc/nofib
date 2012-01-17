-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Encryption.TEA
-- Copyright   :  (c) John Meacham 2008
-- License     :  BSD-style (see the file ReadMe.tex)
--
-- Maintainer  :  john@repetae.net (http://repetae.net/)
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of the TEA tiny encryption algorithm
--
-----------------------------------------------------------------------------

module Codec.Encryption.TEA(
    TEAKey(TEAKey),
    encrypt,
    decrypt
    ) where

import Data.Bits
import Data.Word

-- We don't use LargeKey for both efficiency and practical reasons.

data TEAKey = TEAKey {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32


delta :: Word32
delta = 0x9e3779b9

rounds = 32

encrypt ::  TEAKey -> Word64 -> Word64
encrypt (TEAKey k0 k1 k2 k3) v = f rounds 0 v0 v1 where
    v0,v1 :: Word32
    v0 = fromIntegral v
    v1 = fromIntegral $ v `shiftR` 32
    f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
    f 0 _   v0 v1 = (fromIntegral v1 `shiftL` 32) .|. (fromIntegral v0 .&. 0xffffffff)
    f n sum v0 v1 = f (n - 1) sum' v0' v1' where
        sum' = sum + delta
        v0' = (v0 + (((v1 `shiftL` 4) + k0) `xor` (v1 + sum') `xor` ((v1 `shiftR` 5) + k1)))
        v1' = (v1 + (((v0' `shiftL` 4) + k2) `xor` (v0' + sum') `xor` ((v0' `shiftR` 5) + k3)))


decrypt ::  TEAKey -> Word64 -> Word64
decrypt (TEAKey k0 k1 k2 k3) v = f rounds 0xC6EF3720 v0 v1 where
    v0,v1 :: Word32
    v0 = fromIntegral v
    v1 = fromIntegral $ v `shiftR` 32
    f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
    f 0 _   v0 v1 = (fromIntegral v1 `shiftL` 32) .|. (fromIntegral v0  .&. 0xFFFFFFFF)
    f n sum v0 v1 = f (n - 1) (sum - delta) v0' v1' where
        v1' = (v1 - (((v0 `shiftL` 4) + k2) `xor` (v0 + sum) `xor` ((v0 `shiftR` 5) + k3)))
        v0' = (v0 - (((v1' `shiftL` 4) + k0) `xor` (v1' + sum) `xor` ((v1' `shiftR` 5) + k1)))



{-
 void encrypt (unsigned long* v, unsigned long* k) {
    unsigned long v0=v[0], v1=v[1], sum=0, i;           /* set up */
    unsigned long delta=0x9e3779b9;                     /* a key schedule constant */
    unsigned long k0=k[0], k1=k[1], k2=k[2], k3=k[3];   /* cache key */
    for (i=0; i < 32; i++) {                            /* basic cycle start */
        sum += delta;
        v0 += ((v1<<4) + k0) ^ (v1 + sum) ^ ((v1>>5) + k1);
        v1 += ((v0<<4) + k2) ^ (v0 + sum) ^ ((v0>>5) + k3);  /* end cycle */
    }
    v[0]=v0; v[1]=v1;
}

void decrypt (unsigned long* v, unsigned long* k) {
    unsigned long v0=v[0], v1=v[1], sum=0xC6EF3720, i;  /* set up */
    unsigned long delta=0x9e3779b9;                     /* a key schedule constant */
    unsigned long k0=k[0], k1=k[1], k2=k[2], k3=k[3];   /* cache key */
    for (i=0; i<32; i++) {                              /* basic cycle start */
        v1 -= ((v0<<4) + k2) ^ (v0 + sum) ^ ((v0>>5) + k3);
        v0 -= ((v1<<4) + k0) ^ (v1 + sum) ^ ((v1>>5) + k1);
        sum -= delta;                                   /* end cycle */
    }
    v[0]=v0; v[1]=v1;
}

-}
