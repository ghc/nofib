#
# Makefile for md4
#

INSTALLDIR=/usr/local/bin
MAN_DIR=/usr/man/manl
MAN_SEC=l

PROGS = md4 checkmd4 hashmd4

all: $(PROGS)

md4: md4.o md4driver.o
	$(CC) $(CFLAGS) -o $@ md4.o md4driver.o

checkmd4:      checkmd4.o pipeit.o 
	$(CC) $(CFLAGS) -o $@ checkmd4.o pipeit.o

hashmd4:       hashmd4.o pipeit.o 
	$(CC) $(CFLAGS) -o $@ hashmd4.o pipeit.o

md4.o:		md4.c md4.h md4driver.c
checkmd4.o:     checkmd4.c md4_def.h
hashmd4:	hashmd4.c md4_def.h
pipeit.o:	pipeit.c md4_def.h

clean:
	rm -f md4driver.o md4.o
	rm -f checkmd4.o hashmd4.o pipeit.o

clobber: clean
	rm -f $(PROGS)

print: print_md4 print_tools print_rfc
	cprint README | lpr
	cprint TestArticle | lpr
	cprint Makefile | lpr

print_tools:
	cprint checkmd4.c | lpr
	cprint hashmd4.c  | lpr
	cprint pipeit.c  | lpr
	cprint md4_def.h | lpr
	psroff -man checkmd4.1
	psroff -man hashmd4.1

print_md4:
	cprint md4.h | lpr
	cprint md4.c | lpr
	cprint md4driver.c | lpr

print_rfc:
	cprint rfc1186 | lpr

install:        all
	@echo Installing according to local convention
#	strip md4 checkmd4 hashmd4
#	cp md4 checkmd4 hashmd4 $(INSTALLDIR)
#	chown bin $(INSTALLDIR)/md4 
#	chgrp bin $(INSTALLDIR)/md4 
#	chmod 755 $(INSTALLDIR)/md4 
#	chown bin $(INSTALLDIR)/checkmd4 
#	chgrp bin $(INSTALLDIR)/checkmd4 
#	chmod 755 $(INSTALLDIR)/checkmd4 
#	chown bin $(INSTALLDIR)/hashmd4
#	chgrp bin $(INSTALLDIR)/hashmd4
#	chmod 755 $(INSTALLDIR)/hashmd4
#	cp checkmd4.1 $(MAN_DIR)/checkmd4.$(MAN_SEC)
#	cp hashmd4.1 $(MAN_DIR)/hashmd4.$(MAN_SEC)
#
