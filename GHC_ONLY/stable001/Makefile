TOP = ../../..
PROG = stable001
include $(TOP)/nofib/mk/nofib.mk

Main_C.o: Main_C.c
	$(HC) -c -O Main_C.c

Main_C.c: Main.lhs
	$(LIT2PGM) -r C-code -l c -i Main.lhs -o Main_C.c

Main.hs: Main.lhs
	$(LIT2PGM) -i Main.lhs -o Main.hs
