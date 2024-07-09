SOURCE = paper.qmd

all: html

html: $(SOURCE)
	quarto render $< --to apaquarto-html

pdf: pdf-jou

pdf-man: $(SOURCE)
	quarto render $< --to apaquarto-pdf \
	-M documentmode:man

pdf-doc: $(SOURCE)
	quarto render $< --to apaquarto-pdf \
	-M documentmode:doc

pdf-jou: $(SOURCE)
	quarto render $< --to apaquarto-pdf \
	-M documentmode:jou

docx: $(SOURCE)
	quarto render $< --to apaquarto-docx
