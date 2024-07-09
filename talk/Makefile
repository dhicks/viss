all: talk publish

talk: talk.html
talk.html: talk.qmd custom.scss
	quarto render talk.qmd

.PHONY: publish
publish: talk.html
	quarto publish netlify --id ce1dbe42-3251-4b3a-8e1d-ebb5bcb07afb --no-render

abstract: extd_abstract.pdf
extd_abstract.pdf: extd_abstract.qmd
	quarto render extd_abstract.qmd
