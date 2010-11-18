TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = ../_DphLib/Bench/Time.hs \
       ../_DphLib/Bench/Options.hs \
       ../_DphLib/Bench/Benchmark.hs \
       src/DotPVect.hs \
       src/vect.hs
PROG_ARGS += 50000000
HC_OPTS += -threaded -isrc -i../_DphLib -package base -package dph-par -package dph-prim-par -package old-time -package random
include $(TOP)/mk/target.mk
