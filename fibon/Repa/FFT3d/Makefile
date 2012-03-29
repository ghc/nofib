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
PROG_ARGS += 128 output
HC_OPTS += -threaded -i. -i../_RepaLib/repa -i../_RepaLib/repa-algorithms -i../_RepaLib/repa-io -i../_RepaLib/bmp -i../_RepaLib/repa-bytestring -i../_RepaLib/quickcheck -package base -package binary -package bytestring -package dph-base -package dph-prim-par -package dph-prim-seq -package ghc -package mtl -package old-time -package random -package vector
CLEAN_FILES += output000.bmp output001.bmp output002.bmp output003.bmp output004.bmp output005.bmp output006.bmp output007.bmp output008.bmp output009.bmp output010.bmp output011.bmp output012.bmp output013.bmp output014.bmp output015.bmp output016.bmp output017.bmp output018.bmp output019.bmp output020.bmp output021.bmp output022.bmp output023.bmp output024.bmp output025.bmp output026.bmp output027.bmp output028.bmp output029.bmp output030.bmp output031.bmp output032.bmp output033.bmp output034.bmp output035.bmp output036.bmp output037.bmp output038.bmp output039.bmp output040.bmp output041.bmp output042.bmp output043.bmp output044.bmp output045.bmp output046.bmp output047.bmp output048.bmp output049.bmp output050.bmp output051.bmp output052.bmp output053.bmp output054.bmp output055.bmp output056.bmp output057.bmp output058.bmp output059.bmp output060.bmp output061.bmp output062.bmp output063.bmp output064.bmp output065.bmp output066.bmp output067.bmp output068.bmp output069.bmp output070.bmp output071.bmp output072.bmp output073.bmp output074.bmp output075.bmp output076.bmp output077.bmp output078.bmp output079.bmp output080.bmp output081.bmp output082.bmp output083.bmp output084.bmp output085.bmp output086.bmp output087.bmp output088.bmp output089.bmp output090.bmp output091.bmp output092.bmp output093.bmp output094.bmp output095.bmp output096.bmp output097.bmp output098.bmp output099.bmp output100.bmp output101.bmp output102.bmp output103.bmp output104.bmp output105.bmp output106.bmp output107.bmp output108.bmp output109.bmp output110.bmp output111.bmp output112.bmp output113.bmp output114.bmp output115.bmp output116.bmp output117.bmp output118.bmp output119.bmp output120.bmp output121.bmp output122.bmp output123.bmp output124.bmp output125.bmp output126.bmp output127.bmp
include $(TOP)/mk/target.mk

