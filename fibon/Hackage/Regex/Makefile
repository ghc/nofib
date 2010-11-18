TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = deepseq/Control/DeepSeq.hs \
       regex-base/Text/Regex/Base/RegexLike.hs \
       parsec/Text/ParserCombinators/Parsec/Pos.hs \
       parsec/Text/ParserCombinators/Parsec/Error.hs \
       parsec/Text/ParserCombinators/Parsec/Prim.hs \
       parsec/Text/ParserCombinators/Parsec/Combinator.hs \
       parsec/Text/ParserCombinators/Parsec/Char.hs \
       bitset/Data/BitSet.hs \
       Text/Regex/PDeriv/ExtPattern.lhs \
       parsec/Text/ParserCombinators/Parsec.hs \
       Text/Regex/PDeriv/Dictionary.hs \
       Text/Regex/PDeriv/Common.lhs \
       Text/Regex/PDeriv/Pretty.lhs \
       Text/Regex/PDeriv/RE.lhs \
       Text/Regex/PDeriv/IntPattern.lhs \
       Text/Regex/PDeriv/Translate.lhs \
       Text/Regex/PDeriv/Parse.lhs \
       regex-base/Text/Regex/Base/Context.hs \
       regex-base/Text/Regex/Base.hs \
       Text/Regex/PDeriv/ByteString/LeftToRight.lhs \
       Text/Regex/PDeriv/ByteString/LeftToRightD.lhs \
       Text/Regex/PDeriv/ByteString/Posix.lhs \
       Text/Regex/PDeriv/ByteString/RightToLeft.lhs \
       Text/Regex/PDeriv/ByteString/TwoPasses.lhs \
       bench/Main.hs
PROG_ARGS += 10 addr.txt
HC_OPTS += -i. -ibench -iregex-base -iparsec -ideepseq -ibitset -package array -package base -package bytestring -package containers -package ghc-prim -package mtl -XGADTs -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XTypeSynonymInstances -XFlexibleContexts -XScopedTypeVariables -XDeriveDataTypeable

include $(TOP)/mk/target.mk
