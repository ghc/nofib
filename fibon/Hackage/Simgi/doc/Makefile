
MARKDOWN_FILES = simgi.mdml

doc: build_html build_pdf


build_html: $(MARKDOWN_FILES)
	pandoc -s $< > $(<:.mdml=.html)

build_pdf: $(MARKDOWN_FILES)
	markdown2pdf $<

