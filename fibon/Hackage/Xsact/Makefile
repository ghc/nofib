TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = src/ANSI.lhs \
       src/Indexed.lhs \
       src/Util.hs \
       src/Stats.hs \
       src/Gene.hs \
       src/EST.lhs \
       src/Fasta.lhs \
       src/Suffix.lhs \
       src/Pairs.lhs \
       src/Cluster.lhs \
       src/Clix.lhs \
       src/Xsact.lhs
PROG_ARGS += -O-i -Opdbnt
HC_OPTS += -isrc -package array -package base -package containers -package directory -package process -package random -package unix
include $(TOP)/mk/target.mk
