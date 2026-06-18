SOURCE = paper.qmd
split = 36
split2 = $(shell expr $(split) + 1)

all: floats pdf wc

html: $(SOURCE)
	quarto render $< --to apaquarto-html

pdf: $(SOURCE)
	quarto render $< --to aft-pdf

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
	qpdf paper.pdf --pages . $(split2)-r1 -- supplement.pdf
	pdftotext main.pdf - | wc

# .PHONY: floats
# floats: 
# 	cp ../out/03/03_likert.png img/03_likert.png
# 	cp ../out/03/03_trust.png img/03_trust.png
# 	cp ../out/03/03_vfi.png img/03_vfi.png
# 	cp ../out/03/07_desc_comb.png img/07_desc_comb.png
# 	cp ../out/03/04_trust_coefs.png img/04_trust_coefs.png
# 	cp ../out/03/04_trust_glance.Rds tbl/04_trust_glance.Rds
# 	cp ../out/01/03_loadings_gt.Rds tbl/03_loadings_gt.Rds
# 	cp ../out/03/04_loadings.Rds tbl/04_loadings.Rds
# 	cp ../out/03/04_loadings_bymodel.Rds tbl/04_loadings_bymodel.Rds
# 	cp ../out/03/06_prompts.Rds tbl/06_prompts.Rds
# 	cp ../out/03/05_trust_corr.png img/05_trust_corr.png
# 	cp ../out/03/05_interaction.png img/05_interaction.png
# 	cp ../out/03/05_inter.Rds tbl/05_inter.Rds
# 	cp ../out/03/06_compare.Rds tbl/06_compare.Rds
# 	cp ../out/03/05_factors_coss.png img/05_factors_coss.png
# 	cp ../out/03/03_focal.png img/03_focal.png

.PHONY: floats
floats: img/03_likert.png img/03_trust.png img/03_vfi.png \
        img/07_desc_comb.png img/04_trust_coefs.png img/05_trust_corr.png \
        img/05_interaction.png img/05_factors_coss.png img/03_focal.png \
        img/03_trust_focal.png \
        tbl/04_trust_glance.Rds tbl/03_loadings_gt.Rds tbl/04_loadings.Rds \
        tbl/04_loadings_bymodel.Rds tbl/06_prompts.Rds tbl/05_inter.Rds \
        tbl/06_compare.Rds

img/03_likert.png: ../out/03/03_likert.png
img/03_trust.png: ../out/03/03_trust.png
img/03_vfi.png: ../out/03/03_vfi.png
img/07_desc_comb.png: ../out/03/07_desc_comb.png
img/04_trust_coefs.png: ../out/03/04_trust_coefs.png
img/05_trust_corr.png: ../out/03/05_trust_corr.png
img/05_interaction.png: ../out/03/05_interaction.png
img/05_factors_coss.png: ../out/03/05_factors_coss.png
img/03_focal.png: ../out/03/03_focal.png
img/03_trust_focal.png: ../out/03/03_trust_focal.png
tbl/04_trust_glance.Rds: ../out/03/04_trust_glance.Rds
tbl/03_loadings_gt.Rds: ../out/01/03_loadings_gt.Rds
tbl/04_loadings.Rds: ../out/03/04_loadings.Rds
tbl/04_loadings_bymodel.Rds: ../out/03/04_loadings_bymodel.Rds
tbl/06_prompts.Rds: ../out/03/06_prompts.Rds
tbl/05_inter.Rds: ../out/03/05_inter.Rds
tbl/06_compare.Rds: ../out/03/06_compare.Rds

img/%.png:
	cp $< $@
tbl/%.Rds:
	cp $< $@
