REPORT	= report
OUTPUT	= report.pdf


all:
	latex $(REPORT)
	bibtex $(REPORT)
	latex $(REPORT)
	pdflatex $(REPORT)
	- rm *.aux *.log *.bbl *.blg *.toc *.out
	- xpdf -geometry 1280x900 -z width $(OUTPUT)

clean:
	-rm *.aux *.log *.bbl *.blg *.toc *.out

view:
	xpdf -geometry 1280x900 -z width $(OUTPUT)
