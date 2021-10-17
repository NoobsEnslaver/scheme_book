.PHONY: pdf html latex clean all

all: pdf html latex

latex:
	mkdir -p _build/latex
	skribilo -t latex -o _build/latex/index.tex src/index.skr

pdf:	latex
	mkdir -p _build/pdf
	pdflatex --output-directory=_build/pdf _build/latex/index.tex
	# rm _build/pdf/index.{aux,log,out}

html:
	mkdir -p _build/html
	skribilo -t html -o _build/html/index.html src/index.skr

clean:
	rm -rf _build/
