TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = src/NameSet.hs \
       src/Target.lhs \
       src/AbsSyn.lhs \
       src/ParamRules.hs \
       src/GenUtils.lhs \
       src/ParseMonad.lhs \
       src/Lexer.lhs \
       generated/Parser.hs \
       src/AttrGrammar.lhs \
       generated/AttrGrammarParser.hs \
       src/Grammar.lhs \
       src/First.lhs \
       src/LALR.lhs \
       generated/Paths_happy.hs \
       src/ProduceCode.lhs \
       src/ProduceGLRCode.lhs \
       src/Info.lhs \
       src/Main.lhs
PROG_ARGS += -O-t . Bio.y ErlParser.ly HaskellParser.y TestInput.y
HC_OPTS += -isrc -igenerated -package array -package base -package containers -package mtl -XCPP -XMagicHash
CLEAN_FILES += Bio.hs ErlParser.hs HaskellParser.hs TestInput.hs
include $(TOP)/mk/target.mk
