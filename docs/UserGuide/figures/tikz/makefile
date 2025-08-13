all :
	bash all.bash

%.eps : %.pdf
	pdftops -eps $*

%.pdf : %.tex
	pdflatex $*

%.svg : %.pdf
	lwarpmk pdftosvg $*.pdf

clean :
	rm *~ *.log *.aux *.dvi

superclean :
	rm *.pdf *.svg

#	latex $*
#	dvisvgm $*

