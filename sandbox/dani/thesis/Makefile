REPORT	= thesis
OUTPUT	= thesis.pdf


all:
	pdflatex $(REPORT)
	bibtex $(REPORT)
	pdflatex $(REPORT)
	pdflatex $(REPORT)
	rm -rf *.blg *.bbl *.toc *.out *.log *.aux *.dvi *.mtc* *.maf *.nlo *.brf *.dvi
	- xpdf -geometry 1280x900 -z width $(OUTPUT)

clean:
	rm -rf *.blg *.bbl *.toc *.out *.log *.aux *.dvi *.mtc* *.maf *.nlo *.brf *.dvi

view:
	xpdf -geometry 1280x900 -z width $(OUTPUT)

# create tar 

tar:	
	tar -cpf ~/Desktop/thesis.tar --exclude-vcs -C .. \
	--exclude=thesis/*.{blg,bbl,toc,out,log,aux,mtc*,maf,nlo,brf,dvi} \
	--exclude=thesis/$(OUTPUT) \
	thesis doc withinboundaries_images