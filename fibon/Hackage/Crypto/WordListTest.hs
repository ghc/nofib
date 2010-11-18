{-# LANGUAGE ScopedTypeVariables, PatternSignatures,
             FlexibleInstances, UndecidableInstances,
             TypeSynonymInstances #-}

import Data.LargeWord (Word128, Word192, Word256, LargeKey)
import Codec.Utils (listFromOctets, listToOctets)
import Data.Word (Word8, Word32, Word64)
import Test.QuickCheck

main :: IO ()
main = sequence_ $ map test checks where
    test :: (String, IO ()) -> IO ()
    test (s, t) = do putStrLn $ "Checking " ++ s
                     putStr "        "
                     t

checks :: [(String, IO ())]
checks = [
    ("Word32",
     quickCheck (\(w :: [Word32]) -> (listFromOctets . listToOctets) w == w)),
    ("Word64",
     quickCheck (\(w :: [Word64]) -> (listFromOctets . listToOctets) w == w)),
    ("Word128",
     quickCheck (\(w :: [Word128]) -> (listFromOctets . listToOctets) w == w)),
    ("Word192",
     quickCheck (\(w :: [Word192]) -> (listFromOctets . listToOctets) w == w)),
    ("Word256",
     quickCheck (\(w :: [Word256]) -> (listFromOctets . listToOctets) w == w))]

instance Arbitrary Word8 where
    arbitrary = do
        let mx,mn :: Integer
            mx = fromIntegral (maxBound :: Word8)
            mn = fromIntegral (minBound :: Word8)
        c <- choose (mx, mn)
        return $ fromIntegral c

instance Arbitrary Word32 where
    arbitrary = do
        let mx,mn :: Integer
            mx = fromIntegral (maxBound :: Word32)
            mn = fromIntegral (minBound :: Word32)
        c <- choose (mx, mn)
        return $ fromIntegral c

instance Arbitrary Word64 where
    arbitrary = do
        let mx,mn :: Integer
            mx = fromIntegral (maxBound :: Word64)
            mn = fromIntegral (minBound :: Word64)
        c <- choose (mx, mn)
        return $ fromIntegral c

instance Arbitrary Word128 where
        arbitrary = do
                x <- vector (128 `div` 8) :: Gen [Word8]
                return $ head $ listFromOctets x

instance Arbitrary Word192 where
        arbitrary = do
                x <- vector (192 `div` 8) :: Gen [Word8]
                return $ head $ listFromOctets x

instance Arbitrary Word256 where
        arbitrary = do
                x <- vector (256 `div` 8) :: Gen [Word8]
                return $ head $ listFromOctets x
