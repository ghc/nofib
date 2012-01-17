TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = ../_RepaLib/bmp/Codec/BMP/Base.hs \
       ../_RepaLib/bmp/Codec/BMP/BitmapInfo.hs \
       ../_RepaLib/bmp/Codec/BMP/BitmapInfoV3.hs \
       ../_RepaLib/bmp/Codec/BMP/BitmapInfoV4.hs \
       ../_RepaLib/bmp/Codec/BMP/BitmapInfoV5.hs \
       ../_RepaLib/bmp/Codec/BMP/CIEXYZ.hs \
       ../_RepaLib/bmp/Codec/BMP/Compression.hs \
       ../_RepaLib/bmp/Codec/BMP/Error.hs \
       ../_RepaLib/bmp/Codec/BMP/FileHeader.hs \
       ../_RepaLib/bmp/Codec/BMP.hs \
       ../_RepaLib/bmp/Codec/BMP/Pack.hs \
       ../_RepaLib/bmp/Codec/BMP/Unpack.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/All.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Arbitrary.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Exception.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Function.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Gen.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Modifiers.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Monadic.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Poly.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Property.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/State.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Test.hs \
       ../_RepaLib/quickcheck/Test/QuickCheck/Text.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/Complex.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/Convolve.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/DFT/Center.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/DFT.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/DFT/Roots.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/FFT.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/Iterate.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/Matrix.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/Randomish.hs \
       ../_RepaLib/repa-bytestring/Data/Array/Repa/ByteString.hs \
       ../_RepaLib/repa/Data/Array/Repa/Arbitrary.hs \
       ../_RepaLib/repa/Data/Array/Repa.hs \
       ../_RepaLib/repa/Data/Array/Repa/Index.hs \
       ../_RepaLib/repa/Data/Array/Repa/Internals/Base.hs \
       ../_RepaLib/repa/Data/Array/Repa/Internals/Elt.hs \
       ../_RepaLib/repa/Data/Array/Repa/Internals/EvalBlockwise.hs \
       ../_RepaLib/repa/Data/Array/Repa/Internals/EvalChunked.hs \
       ../_RepaLib/repa/Data/Array/Repa/Internals/EvalCursored.hs \
       ../_RepaLib/repa/Data/Array/Repa/Internals/EvalReduction.hs \
       ../_RepaLib/repa/Data/Array/Repa/Internals/Forcing.hs \
       ../_RepaLib/repa/Data/Array/Repa/Internals/Gang.hs \
       ../_RepaLib/repa/Data/Array/Repa/Internals/Select.hs \
       ../_RepaLib/repa/Data/Array/Repa/Operators/IndexSpace.hs \
       ../_RepaLib/repa/Data/Array/Repa/Operators/Interleave.hs \
       ../_RepaLib/repa/Data/Array/Repa/Operators/Mapping.hs \
       ../_RepaLib/repa/Data/Array/Repa/Operators/Modify.hs \
       ../_RepaLib/repa/Data/Array/Repa/Operators/Reduction.hs \
       ../_RepaLib/repa/Data/Array/Repa/Operators/Select.hs \
       ../_RepaLib/repa/Data/Array/Repa/Operators/Traverse.hs \
       ../_RepaLib/repa/Data/Array/Repa/Properties.hs \
       ../_RepaLib/repa/Data/Array/Repa/Shape.hs \
       ../_RepaLib/repa/Data/Array/Repa/Slice.hs \
       ../_RepaLib/repa/Data/Array/Repa/Specialised/Dim2.hs \
       ../_RepaLib/repa/Data/Array/Repa/Stencil/Base.hs \
       ../_RepaLib/repa/Data/Array/Repa/Stencil.hs \
       ../_RepaLib/repa/Data/Array/Repa/Stencil/Template.hs \
       ../_RepaLib/repa-io/Data/Array/Repa/IO/Binary.hs \
       ../_RepaLib/repa-io/Data/Array/Repa/IO/BMP.hs \
       ../_RepaLib/repa-io/Data/Array/Repa/IO/ColorRamp.hs \
       ../_RepaLib/repa-io/Data/Array/Repa/IO/Internals/Text.hs \
       ../_RepaLib/repa-io/Data/Array/Repa/IO/Matrix.hs \
       ../_RepaLib/repa-io/Data/Array/Repa/IO/Timing.hs \
       ../_RepaLib/repa-io/Data/Array/Repa/IO/Vector.hs \
       Main.hs
PROG_ARGS += 12 lena.bmp out.bmp
HC_OPTS += -threaded -i. -i../_RepaLib/repa -i../_RepaLib/repa-algorithms -i../_RepaLib/repa-io -i../_RepaLib/bmp -i../_RepaLib/repa-bytestring -i../_RepaLib/quickcheck -package base -package binary -package bytestring -package dph-base -package dph-prim-par -package dph-prim-seq -package extensible-exceptions -package ghc -package mtl -package old-time -package random -package vector
CLEAN_FILES += out.bmp
include $(TOP)/mk/target.mk

