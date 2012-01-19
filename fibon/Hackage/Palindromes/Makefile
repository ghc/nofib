TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = src/Data/Algorithms/Palindromes/Palindromes.hs \
       src/Data/Algorithms/Palindromes/Options.hs \
       src/Data/Algorithms/Palindromes/Main.hs
PROG_ARGS += -s annakarenina.txt huckfinn.txt olivertwist.txt swannsway.txt +RTS -K128M -RTS
HC_OPTS += -isrc -package array -package base
include $(TOP)/mk/target.mk
