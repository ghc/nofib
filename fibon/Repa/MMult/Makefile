TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = ../_RepaLib/repa-io/Data/Array/Repa/IO/Internals/Text.hs \
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
       ../_RepaLib/repa-io/Data/Array/Repa/IO/Timing.hs \
       ../_RepaLib/repa/Data/Array/Repa.hs \
       ../_RepaLib/repa-io/Data/Array/Repa/IO/Matrix.hs \
       ../_RepaLib/repa-algorithms/Data/Array/Repa/Algorithms/Matrix.hs \
       src/MMult/src/Main.hs
PROG_ARGS += -random 1000 1000 -random 1000 1000
HC_OPTS += -threaded -i. -i../_RepaLib/repa -i../_RepaLib/repa-algorithms -i../_RepaLib/repa-io -i../_RepaLib/bmp -i../_RepaLib/repa-bytestring -i../_RepaLib/quickcheck -package base -package binary -package bytestring -package dph-base -package dph-prim-par -package dph-prim-seq -package extensible-exceptions -package ghc -package mtl -package old-time -package random
include $(TOP)/mk/target.mk
