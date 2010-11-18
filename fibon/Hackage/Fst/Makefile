TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = src/FST/GetOpt.hs \
       src/FST/Alex.hs \
       src/FST/Lexer.hs \
       src/FST/RegTypes.hs \
       src/FST/AutomatonTypes.hs \
       src/FST/StateMonad.hs \
       src/FST/Utils.hs \
       src/FST/Automaton.hs \
       src/FST/Complete.hs \
       src/FST/Deterministic.hs \
       src/FST/Reversal.hs \
       src/FST/MinimalBrzozowski.hs \
       src/FST/LBFA.hs \
       src/FST/AutomatonInterface.hs \
       src/FST/TransducerTypes.hs \
       src/FST/Transducer.hs \
       src/FST/RunTransducer.hs \
       src/FST/DeterministicT.hs \
       src/FST/ReversalT.hs \
       src/FST/MinimalTBrzozowski.hs \
       src/FST/EpsilonFreeT.hs \
       src/FST/Arguments.hs \
       src/FST/RRegTypes.hs \
       src/FST/LBFT.hs \
       src/FST/NReg.hs \
       src/FST/Parse.hs \
       src/FST/FileImport.hs \
       src/FST/TransducerInterface.hs \
       src/FST/Info.hs \
       src/FST/Main.hs
PROG_ARGS += -u soda.fst
STDIN_FILE = PLONK
HC_OPTS += -isrc -package array -package base -package haskell98
include $(TOP)/mk/target.mk
