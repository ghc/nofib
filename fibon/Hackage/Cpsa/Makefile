TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = generated/Paths_cpsa.hs \
       src/CPSA/Lib/Pretty.hs \
       src/CPSA/Lib/Utilities.hs \
       src/CPSA/DiffieHellman/IntLinEq.hs \
       src/CPSA/Lib/SExpr.hs \
       src/CPSA/Lib/Algebra.hs \
       src/CPSA/Lib/Printer.hs \
       src/CPSA/Lib/Entry.hs \
       src/CPSA/Lib/Protocol.hs \
       src/CPSA/Lib/Strand.hs \
       src/CPSA/Lib/Cohort.hs \
       src/CPSA/Lib/Loader.hs \
       src/CPSA/Lib/Displayer.hs \
       src/CPSA/Lib/CPSA.hs \
       src/CPSA/Lib/Expand.hs \
       src/CPSA/Basic/Algebra.hs \
       src/CPSA/DiffieHellman/Algebra.hs \
       src/CPSA/Lib/Main.hs
PROG_ARGS += -x1 nsl5.lisp
HC_OPTS += -fcontext-stack=42 -isrc -igenerated -package base -package containers
include $(TOP)/mk/target.mk
