.PHONY: pdf html latex clean all

all: pdf html

latex:
	mkdir -p _build/latex
	skribilo -c minted-output-dir=_build/pdf -t latex -o _build/latex/index.tex src/index.skr

pdf:	latex
	mkdir -p _build/pdf
	xelatex -interaction nonstopmode -shell-escape --output-directory=_build/pdf _build/latex/index.tex
	# rm _build/pdf/index.{aux,log,out}

html:
	mkdir -p _build/html
	skribilo -t html -o _build/html/index.html src/index.skr

clean:
	rm -rf _build/
