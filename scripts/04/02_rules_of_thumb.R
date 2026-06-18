## =============================================================================
## 02_rules_of_thumb.R — Conventional sample-size heuristics for the Study 4 CFA
##
## TECHNICAL NOTE FOR REVIEWING COAUTHOR
## -----------------------------------------------------------------------------
## Purpose
##   A cross-check on the Monte Carlo analysis (01_cfa_power_sim.R). The Monte
##   Carlo study is the primary, model-specific justification; this script just
##   tabulates the familiar "rules of thumb" so reviewers can see the simulation
##   lands in (or above) the conventional range. These heuristics are crude --
##   they ignore the actual loadings -- but they are widely cited.
##
## Model facts (finalized 3-factor VISS, from the Study 3 CFA):
##   - q = 23 freely estimated parameters
##   - 10 indicators, 3 factors
##   - several low-communality indicators (loadings ~0.3-0.4 for vfi.1, aims.1,
##     pluralism.1), which per MacCallum, Widaman, Zhang & Hong (1999) pushes the
##     required N *upward* -- consistent with the Monte Carlo result.
##
## Output (out/04/)
##   02_rules_of_thumb.Rds / .html : the heuristics table
## =============================================================================

library(tidyverse)
library(lavaan)
library(tinytable)
library(here)
library(glue)

data_dir = here('data', '03')
out_dir = here('out', '04')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## Recompute model facts from the population fit (keep this script self-contained)
pop_spec = '
  textbook    =~ consensus.2 + fallible.1 + pluralism.1
  cynicism    =~ coi.1 + coi.2 + consensus.1 + aims.1
  objectivity =~ nonsubj.1 + nonsubj.2 + vfi.1
'
viss_df = here(data_dir, '01_data.Rds') |>
  read_rds() |>
  select(starts_with('viss')) |>
  rename_with(~ str_remove(.x, 'viss.'))

pop_fit = cfa(pop_spec, data = viss_df)
q = lavInspect(pop_fit, 'npar')                       # free parameters
p = length(lavNames(pop_fit, 'ov'))                   # indicators

## Heuristics table ----
rules_tbl = tribble(
  ~ Rule, ~ Prescription, ~ `Implied N`, ~ Source,
  'Absolute minimum',
    'N >= 200 for SEM/CFA', '200',
    'Boomsma (1985); Boomsma & Hoogland (2001)',
  'N : q ratio (lower)',
    glue('10 x {q} free parameters'), glue('{10 * q}'),
    'Bentler & Chou (1987)',
  'N : q ratio (upper)',
    glue('20 x {q} free parameters'), glue('{20 * q}'),
    'Jackson (2003)',
  'N per indicator',
    glue('10-20 x {p} indicators'), glue('{10 * p}-{20 * p}'),
    'Kline (2016)',
  'Communality-based',
    'Larger N when communalities are low and factors are not highly overdetermined; several VISS items have low communalities',
    '>= 300 (qualitative)',
    'MacCallum, Widaman, Zhang & Hong (1999)'
)

rules_tt = tt(rules_tbl,
              caption = 'Conventional sample-size heuristics for the finalized 3-factor VISS CFA') |>
  style_tt(j = 1, bold = TRUE) |>
  theme_tt('striped')

rules_tt

save_tt(rules_tt, here(out_dir, '02_rules_of_thumb.html'), overwrite = TRUE)
write_rds(rules_tt, here(out_dir, '02_rules_of_thumb.Rds'))

cat(glue('\nModel: q = {q} free parameters, p = {p} indicators\n'))
cat('Heuristic range: roughly 200-460; the low-communality caveat argues for the upper end.\n')
