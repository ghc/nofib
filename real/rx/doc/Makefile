MAIN = rxdoc


RX = ../src/RX

RX_OPTS = 

RTS_OPTS = +RTS -H50M -K10M -RTS

WWW = $(HOME)/.www/rx

###########################################################################

all : $(MAIN).tex $(MAIN).dvi $(MAIN).ps $(MAIN).dvi.gz $(MAIN).ps.gz

clean :
	rm -f *~ *.tex *.dvi *.aux *.toc *.log *.ps

%.gz : %
	gzip -v -9 $<

# latex stuff ##############################################################


%.tex : %.lit 
	$(RX) $(RTS_OPTS) $(RX_OPTS) current=text code=latex $< |tee $@

%.dvi : %.tex
	- latex "\\nonstopmode\\input $<"

%.ps : %.dvi
	dvips -t a4 $< -o $@

