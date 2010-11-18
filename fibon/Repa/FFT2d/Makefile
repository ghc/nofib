TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = ../_RepaLib/bmp/Codec/BMP/CIEXYZ.hs \
       ../_RepaLib/bmp/Codec/BMP/Error.hs \
       ../_RepaLib/bmp/Codec/BMP/BitmapInfoV3.hs \
       ../_RepaLib/bmp/Codec/BMP/FileHeader.hs \
       ../_RepaLib/bmp/Codec/BMP/BitmapInfoV4.hs \
       ../_RepaLib/bmp/Codec/BMP/BitmapInfoV5.hs \
       ../_RepaLib/bmp/Codec/BMP/BitmapInfo.hs \
       ../_RepaLib/bmp/Codec/BMP/Base.hs \
       ../_RepaLib/bmp/Codec/BMP/Unpack.hs \
       ../_RepaLib/bmp/Codec/BMP/Pack.hs \
       ../_RepaLib/bmp/Codec/BMP.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Exception.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Text.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/State.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Gen.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Arbitrary.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Modifiers.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Property.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Test.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck.hs \
       ../_RepaLib/repa/Data/Array/Repa/QuickCheck.hs \
       ../_RepaLib/repa/Data/Array/Repa/Shape.hs \
       ../_RepaLib/repa/Data/Array/Repa/Index.hs \
       ../_RepaLib/repa/Data/Array/Repa/Slice.hs \
       ../_RepaLib/repa/Data/Array/Repa.hs \
       ../_RepaLib/repa-bytestring/Data/Array/Repa/ByteString.hs \
       ../_RepaLib/repa-io/Data/Array/Repa/IO/Timing.hs \
       ../_RepaLib/repa-io/Data/Array/Repa/IO/BMP.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/Complex.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/DFT/Center.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/FFT.hs \
       src/FFT/HighPass2d/src/Main.hs
PROG_ARGS += 1 lena.bmp out.bmp
HC_OPTS += -threaded -i. -i../_RepaLib/repa -i../_RepaLib/repa-algorithms -i../_RepaLib/repa-io -i../_RepaLib/bmp -i../_RepaLib/repa-bytestring -i../_RepaLib/quickcheck -package base -package binary -package bytestring -package dph-base -package dph-prim-par -package dph-prim-seq -package extensible-exceptions -package ghc -package mtl -package old-time -package random
CLEAN_FILES += out.bmp
include $(TOP)/mk/target.mk
