TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = Test/QuickCheck/Exception.hs \
       Test/QuickCheck/Text.hs \
       Test/QuickCheck/State.hs \
       Test/QuickCheck/Gen.hs \
       Test/QuickCheck/Arbitrary.hs \
       Test/QuickCheck/Property.hs \
       Test/QuickCheck/Modifiers.hs \
       Test/QuickCheck/Test.hs \
       Test/QuickCheck.hs \
       examples/ExSimple.hs \
       examples/ExSet.hs \
       examples/ExMerge.hs \
       examples/ExLambda.hs \
       examples/ExHeap.hs \
       examples/Main.hs
PROG_ARGS += 100000
HC_OPTS += -iexamples -i. -package base -package containers -package ghc -package mtl -package random
include $(TOP)/mk/target.mk
