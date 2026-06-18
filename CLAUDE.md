# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Research compendium for the paper *"Developing Measures of Public Perceptions of Values in Science: The Values in Science Scale"* (Hicks, Lobato, Campbell, Dad). It contains the survey data, R analysis pipeline, and Quarto sources for the paper, conference talk, and poster. The deliverable is a manuscript, not a software package (the `DESCRIPTION` is a vestigial RStudio-project stub).

The project covers three studies. Numbered subdirectories map to studies: `01` = Study 1 (initial 36-item ViS item pool, exploratory factor analysis), `03` = Study 3 (the validated VISS instrument and trust analyses). Study 2 was qualitative and has no analysis scripts.

## Build commands

Builds are driven by `make` + Quarto. The root `Makefile` delegates to subdirectory Makefiles (`pipe` → `scripts/`, `paper` → `paper/`, `talk` → `talk/`).

```sh
make             # full build: pipeline, talk, paper
cd paper && make # render paper.pdf (aft-pdf format), copy floats, split + word-count
cd talk  && make # render and publish the reveal.js talk
```

Key `paper/` targets:
- `make floats` — copies generated figures/tables from `../out/{01,03}/` into `paper/img/` and `paper/tbl/`. **Run this after re-running analysis scripts** so the paper picks up refreshed outputs.
- `make pdf` — `quarto render paper.qmd --to aft-pdf` (the submission format; `aft.cls` is the *Annals* journal class).
- `make wc` — splits `paper.pdf` into `main.pdf` / `supplement.pdf` at a hardcoded page (`split = 36`) via `qpdf` and word-counts the main text. Update `split` if pagination changes.

To render the paper directly: `quarto render paper/paper.qmd --to aft-pdf`. Other registered formats: `apaquarto-html`, `apaquarto-pdf` (with `documentmode` man/doc/jou), `apaquarto-docx`.

## Analysis pipeline

Scripts in `scripts/01/` and `scripts/03/` are **run in numeric order** and form a pipeline: `01_clean` → `02_*` (descriptive/eda) → `03_*` (descriptives, scale) → `04_viss_efa` → `05_trust` → `06_prompts_gt` → `07_desc_combined`. There is no script-level Makefile automation (`scripts/Makefile` is a stub); run scripts manually in order.

Data flow:
- **Inputs:** `data/01/` and `data/03/` hold cleaned, anonymized `.Rds`/`.csv` (already produced by each study's `01_clean.R` from raw Qualtrics/Prolific exports).
- **Outputs:** scripts write figures (`.png`), tables (`.Rds`/`.html`/`.tex`/`.pdf`), and intermediate model objects to `out/01/` and `out/03/`.
- **Paper:** `paper/Makefile`'s `floats` target stages selected `out/` artifacts into `paper/img/` and `paper/tbl/`, which `paper.qmd` then reads.

So the end-to-end loop is: edit script → re-run affected scripts in order → `cd paper && make floats` → `make pdf`.

### Conventions used across scripts

- Paths are always resolved with `here::here(...)` from the project root; scripts set `data_dir`/`out_dir` near the top. Preserve this — do not hardcode relative paths.
- Shared helpers live in `R/` and are pulled in with `source(here('R', '...'))`, not via a package namespace. Notably: `vars.R` (canonical variable groupings: `viss_vars`, `trust_vars`, `demo_cont`, `demo_cat` as `rlang::expr` lists), `vis_labels.R` (item tag/label lookup), `scores.R`, `loading_table.R`, `highlight_cells.R`, `div_bar.R`, `effects_plot.R`, `reg_plots.R`, `plot_adjustments.R`.
- Style is tidyverse-heavy with the native `|>` pipe; `set.seed()` is used before any random split (e.g. EFA/CFA sample splitting) — keep seeds fixed for reproducibility.
- Anonymization: `01_clean.R` hashes participant IDs with `openssl::sha224` keyed from `secrets/key.R`. `secrets/` is local-only; do not commit it or print the key. Survey IDs live in `data/03/survey_id.R`.

### Reproducibility caveat

`renv`/`renv.lock` was used early but **is no longer maintained** (`.Rprofile` has `renv/activate.R` commented out), because later analysis steps needed newer package versions. The full pipeline is **not** guaranteed to reproduce from scratch. When running scripts, use the currently installed package versions rather than trying to restore `renv`.

## Layout quick reference

- `scripts/` — analysis pipeline (`01/`, `03/`); `scripts/scratch/` is exploratory, not part of the build.
- `R/` — sourced helper functions and shared variable definitions.
- `data/` — cleaned, anonymized study data.
- `out/` — generated figures, tables, and model objects.
- `paper/` — `paper.qmd` manuscript + `VISS.yaml` bibliography; `img/`/`tbl/` hold staged floats; `*BJPS`, `*PUS`, `*SciComm` dirs are journal-submission variants/snapshots.
- `talk/`, `poster/` — conference outputs.
- `materials/`, `out/VISS_alone.qsf` — survey instrument materials.
