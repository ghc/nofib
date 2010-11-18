TOP = ../../..
include $(TOP)/mk/boilerplate.mk
HSC2HS_INPLACE=$(GHC_TOP)/$(INPLACE_BIN)/$(GHC_HSC2HS_PGM)
HSC2HS_OPTS += --cflag=-Imersenne-random-pure64/include
SRCS = parsec-2/Text/ParserCombinators/Parsec/Pos.hs \
       parsec-2/Text/ParserCombinators/Parsec/Error.hs \
       parsec-2/Text/ParserCombinators/Parsec/Prim.hs \
       parsec-2/Text/ParserCombinators/Parsec/Combinator.hs \
       parsec-2/Text/ParserCombinators/Parsec/Char.hs \
       mersenne-random-pure64/System/Random/Mersenne/Pure64/Base.hs \
       mersenne-random-pure64/System/Random/Mersenne/Pure64/MTBlock.hs \
       mersenne-random-pure64/System/Random/Mersenne/Pure64.hs \
       src/RpnData.hs \
       src/GenericModel.hs \
       src/RpnCalc.hs \
       src/ExtraFunctions.hs \
       parsec-2/Text/ParserCombinators/Parsec.hs \
       parsec-2/Text/ParserCombinators/Parsec/Token.hs \
       parsec-2/Text/ParserCombinators/Parsec/Language.hs \
       src/TokenParser.hs \
       src/RpnParser.hs \
       src/Messages.hs \
       src/IO.hs \
       src/InputCheck.hs \
       src/InputParser.hs \
       src/Engine.hs \
       src/CommandLine.hs \
       src/simgi.hs \
       mersenne-random-pure64/cbits/mt19937-64-block.c \
       mersenne-random-pure64/cbits/mt19937-64-unsafe.c \
       mersenne-random-pure64/cbits/mt19937-64.c
PROG_ARGS += oregonator.sgl
HC_OPTS += -isrc -igenerated -iparsec-2 -imersenne-random-pure64 -package base -package containers -package haskell98 -package mtl -package old-time -package random -XExistentialQuantification -XPolymorphicComponents
CC_OPTS += $(CONF_CC_OPTS_STAGE2) -Imersenne-random-pure64/include
CLEAN_FILES += oregonator_output.dat mersenne-random-pure64/System/Random/Mersenne/Pure64/Base.hs mersenne-random-pure64/System/Random/Mersenne/Pure64/Base_hsc.c
include $(TOP)/mk/target.mk
