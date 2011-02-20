REPORT	= thesis
OUTPUT	= thesis.pdf


all:
	latex $(REPORT)
	bibtex $(REPORT)
	latex $(REPORT)
	pdflatex $(REPORT)
	rm -rf *.{blg,bbl,toc,out,log,aux,mtc*,maf,nlo,brf,dvi}
	- xpdf -geometry 1280x900 -z width $(OUTPUT)

clean:
	rm -rf *.{blg,bbl,toc,out,log,aux,dvi,mtc*,maf,nlo,brf,dvi}

view:
	xpdf -geometry 1280x900 -z width $(OUTPUT)