REPORT	= slide_thesis
OUTPUT	= slide_thesis.pdf


all:
	pdflatex $(REPORT)
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
	tar -cpf ~/Desktop/slide.thesis.tar --exclude-vcs -C .. \
	--exclude=slide_thesis/*.{blg,bbl,toc,out,log,aux,dvi,mtc*,maf,nlo,brf,dvi,nav,snm,vrb} \
	--exclude=slide_thesis/$(OUTPUT) \
	slide_thesis