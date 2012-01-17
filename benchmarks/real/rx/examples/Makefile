TOP = ../../../..
include $(TOP)/mk/boilerplate.mk

SRC_RUNTEST_OPTS += +RTS -H50M -K10M -RTS basic.lit check.lit -o1 rx.stdout

# compiled version
NOFIB_PROG= ../src/rx

# interpreted version
# RX = runhugs -h1m -l ../src/RX.hs

all :: runtests

###########################################################################

clean ::
	rm *~ 

# latex stuff ##############################################################


%.tex : %.lit 
	$(RX) $(RTS_OPTS) $(RX_OPTS) current=text code=latex $< |tee $@

%.out : %.in
	$(RX) $(RTS_OPTS) $(RX_OPTS) current=code code=plain $< |tee $@

%.dvi : %.tex
	- latex "\\nonstopmode\\input $<"

%.ps : %.dvi
	dvips -t a4 $< -o $@


include $(TOP)/mk/target.mk
