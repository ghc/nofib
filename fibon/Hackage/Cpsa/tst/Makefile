# When the binary changes, run the test suite, and look for changes in
# the output.  The changes are not fatal errors, but for a *.sc{m,h} input
# file, an error produced by the binary is fatal.

TSTS	:= $(patsubst %.scm,%.txt,$(wildcard *.scm)) \
		$(patsubst %.lsp,%.txt,$(wildcard *.lsp)) \
		$(patsubst %.sch,%.txt,$(wildcard *.sch))
PROG	= ../dist/build/cpsa/cpsa
CPSAFLAGS = +RTS -M512m -RTS

# The expected answers are in files with the .tst extension.

# Analyze protocols for shapes expecting success
%.txt:	%.scm $(PROG)
	$(PROG) $(CPSAFLAGS) -o $@ $*.scm
	-diff -u $*.tst $*.txt

# Analyze protocols for shapes expecting failure
%.txt:	%.lsp $(PROG)
	-$(PROG) $(CPSAFLAGS) -o $@ $*.lsp
	-diff -u $*.tst $*.txt

# Analyze protocols for shapes expecting success using Diffie-Hellman
%.txt:	%.sch $(PROG)
	$(PROG) $(CPSAFLAGS) -a diffie-hellman -o $@ $*.sch
	-diff -u $*.tst $*.txt

# Extract shapes
%_shapes.txt:	%.txt
	../dist/build/cpsashapes/cpsashapes -o $@ $<

# Annotate shapes
%_annotations.txt:	%_shapes.txt
	../dist/build/cpsaannotations/cpsaannotations -o $@ $<

# Visualize output
%.svg:		%.txt
	../dist/build/cpsagraph/cpsagraph -c -o $@ $<

# Visualize output using the expanded format
%.xml:		%.txt
	../dist/build/cpsagraph/cpsagraph -o $@ $<

.PRECIOUS:	%.txt %_shapes.txt %_annotations.txt

all:	$(TSTS) $(patsubst %.txt,%.xml,$(TSTS)) \
		$(patsubst %.txt,%_shapes.xml,$(TSTS))

clean:
	-rm *.txt *.xml
