TOP = ../../..
include $(TOP)/mk/boilerplate.mk
SRCS = src/Pos.hs \
       src/Parse.hs \
       java-parser/JavaMonad.hs
PROG_ARGS += Scar.java SPEED.java Blowfish.java CAST5.java DES.java TestRijndael.java
HC_OPTS += -isrc -ijava-parser -package base -package haskell98
include $(TOP)/mk/target.mk
