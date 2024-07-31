SOURCE = paper.qmd

all: pdf

html: $(SOURCE)
	quarto render $< --to apaquarto-html

pdf: pdf-doc wc

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

.PHONY: wc
split = 19
wc:
	echo "splitting at page $(split)"
	qpdf paper.pdf --pages . 1-$(split) -- main.pdf
	pdftotext main.pdf - | wc

.PHONY: floats
floats: 
	cp ../out/03/02_likert.png img/02_likert.png
	cp ../out/03/02_trust.png img/02_trust.png
	cp ../out/03/02_vfi.png img/02_vfi.png
	cp ../out/03/07_desc_comb.png img/07_desc_comb.png
	cp ../out/03/04_trust_coefs.png img/04_trust_coefs.png
	cp ../out/01/03_loadings_gt.Rds tbl/03_loadings_gt.Rds
	cp ../out/03/04_loadings.Rds tbl/04_loadings.Rds
	cp ../out/03/06_prompts.Rds tbl/06_prompts.Rds

