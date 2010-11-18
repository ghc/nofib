TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = HaLeX_lib/Language/HaLex/Util.hs \
       HaLeX_lib/Language/HaLex/Parser.hs \
       HaLeX_lib/Language/HaLex/Dfa.hs \
       HaLeX_lib/Language/HaLex/Ndfa.hs \
       HaLeX_lib/Language/HaLex/FaOperations.hs \
       HaLeX_lib/Language/HaLex/Minimize.hs \
       HaLeX_lib/Language/HaLex/RegExp.hs \
       HaLeX_lib/Language/HaLex/RegExpParser.hs \
       HaLeX_lib/Language/HaLex/RegExp2Fa.hs \
       HaLeX_lib/Language/HaLex/Equivalence.hs \
       HaLeX_lib/Language/HaLex/FaAsDiGraph.hs \
       HaLeX_lib/Language/HaLex/RegExpAsDiGraph.hs \
       HaLeX_lib/Language/HaLex/Dfa2MDfa.hs \
       HaLeX_tool/halex.hs
PROG_ARGS += real
HC_OPTS += -iHaLeX_tool -iHaLeX_lib -package base -package mtl
include $(TOP)/mk/target.mk
