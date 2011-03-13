REPORT	= slide_thesis
OUTPUT	= slide_thesis.pdf


all:
	pdflatex $(REPORT)
	pdflatex $(REPORT)
	rm -rf *.blg *.bbl *.toc *.out *.log *.aux *.dvi *.mtc* *.maf *.nlo *.brf *.dvi *.nav *.snm *.vrb
	- xpdf -geometry 1280x900 -z page $(OUTPUT)

clean:
	rm -rf *.blg *.bbl *.toc *.out *.log *.aux *.dvi *.mtc* *.maf *.nlo *.brf *.dvi *.nav *.snm *.vrb *.pdf

view:
	xpdf -geometry 1280x900 -z page $(OUTPUT)

# create tar 

tar:	
	tar -cpf ~/Desktop/thesis.tar.gz --exclude-vcs -C .. \
	--exclude=thesis/*.{blg,bbl,toc,out,log,aux,mtc*,maf,nlo,brf,dvi} \
	--exclude=thesis/$(OUTPUT) \
	thesis doc withinboundaries_images