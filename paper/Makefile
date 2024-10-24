SOURCE = paper.qmd
split = 19

all: pdf

html: $(SOURCE)
	quarto render $< --to apaquarto-html

pdf: $(SOURCE)
	quarto render $< --to pdf

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
	cp ../out/03/04_trust_glance.Rds tbl/04_trust_glance.Rds
	cp ../out/01/03_loadings_gt.Rds tbl/03_loadings_gt.Rds
	cp ../out/03/04_loadings.Rds tbl/04_loadings.Rds
	cp ../out/03/04_loadings_bymodel.Rds tbl/04_loadings_bymodel.Rds
	cp ../out/03/06_prompts.Rds tbl/06_prompts.Rds
	cp ../out/03/05_trust_corr.png img/05_trust_corr.png
	cp ../out/03/05_interaction.png img/05_interaction.png
	cp ../out/03/05_inter.Rds tbl/05_inter.Rds
	cp ../out/03/06_compare.Rds tbl/06_compare.Rds

