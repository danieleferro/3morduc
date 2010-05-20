REPORT	= report
OUTPUT	= report.pdf

all:
	latex $(REPORT)
	bibtex $(REPORT)
	latex $(REPORT)
	latex $(REPORT)
	pdflatex $(REPORT)
	- rm *.aux *.log *.bbl *.blg *.toc
	- xpdf $(OUTPUT)

clean:
	-rm *.aux *.log *.bbl *.blg *.toc

view:
	evince $(OUTPUT)

