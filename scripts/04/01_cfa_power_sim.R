## =============================================================================
## 01_cfa_power_sim.R — Monte Carlo sample-size analysis for a Study 4 CFA
##
## TECHNICAL NOTE FOR REVIEWING COAUTHOR
## -----------------------------------------------------------------------------
## Purpose
##   Determine how many respondents Study 4 needs to confirm (via CFA) the
##   finalized 3-factor VISS structure that Study 3 identified by EFA. Study 3
##   (N = 502) was too small to split into EFA + CFA halves, so reviewers asked
##   for a properly powered confirmatory replication.
##
## Why this is NOT just "Study 3 was N ~ 500, so go bigger"
##   Study 3's constraint was the *split*, not the raw N. Doing EFA and CFA on
##   one dataset requires holding out a CFA half, which left Study 3 with ~250
##   per half; at that size the sampling-adequacy check (KMO) fell to "middling"
##   (0.72), so the team ran EFA only, on the whole sample. Study 4 does NOT need
##   to split: the factor structure is already fixed from Study 3, so Study 4 is
##   purely confirmatory and its *entire* sample feeds the CFA. That is why a
##   Study 4 of N ~ 400 (unsplit) is in far better shape for a CFA than Study 3's
##   ~250 split half. The n_grid below brackets the answer; the recommendation
##   lands comfortably inside it (so the upper end is adequate, not a ceiling we
##   bumped into).
##
## Why simulation rather than a power formula
##   "Power" for a CFA is not a single quantity. Closed-form (RMSEA-based)
##   methods test only *overall model fit*. What we actually need is that the
##   individual loadings and factor correlations are estimated with low bias,
##   correct standard errors, and adequate power. The standard tool for that is
##   a Monte Carlo study (Muthen & Muthen 2002, "How to use a Monte Carlo study
##   to decide on sample size and determine power").
##
## Design
##   Population model:
##     - The finalized 3-factor VISS model (see pop_spec below), with population
##       parameter values taken from the Study 3 ML CFA fit on the full sample
##       (scripts/03/04_viss_efa.R fits the same 3-factor structure). We fit it
##       here with the default marker-variable scaling (first indicator loading
##       fixed to 1). Marker scaling fixes each factor's orientation, which
##       prevents sign-flips of whole factors across replications.
##   Data generation:
##     - lavaan::simulateData() draws multivariate-normal data from the
##       population parameter values. Items are treated as continuous (ML),
##       matching how Study 3 analyzed the 7-point Likert items. (An ordinal /
##       WLSMV sensitivity check could be added later; it generally needs a
##       somewhat larger N.)
##   For each candidate sample size n in n_grid, we draw n_reps datasets, refit
##   the model, and record per-parameter estimates, SEs, p-values, CIs, plus
##   convergence and admissibility (no negative variances / Heywood cases).
##
## Acceptance criteria (Muthen & Muthen 2002)
##   For the parameters of interest (the freely estimated loadings and the three
##   factor covariances), at the recommended n we require:
##     - convergence + admissibility  >= 0.95
##     - |parameter bias|             <= 10%
##     - |standard-error bias|        <= 10%
##     - 95% CI coverage              in [0.91, 0.98]
##     - power (P[param significant]) >= 0.80
##   The recommended N is the smallest n in the grid meeting all of these.
##
## Outputs (out/04/)
##   01_pop_syntax.txt     population model with baked-in parameter values
##   01_power_summary.Rds  per-(n, parameter) summary tibble
##   01_power_summary.csv  same, human-readable
##   01_power_curve.png    power vs. n for the parameters of interest
##   Printed to console: the recommended N and the binding parameters.
## =============================================================================

library(tidyverse)
library(lavaan)
library(furrr)
library(here)
library(glue)
library(cli)
library(assertthat)

theme_set(theme_bw())

data_dir = here('data', '03')
out_dir = here('out', '04')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## Simulation settings ----
n_grid = c(150, 200, 250, 300, 350, 400, 450, 500)
n_reps = 1000
base_seed = 20260618

## Population model from Study 3 ----
## The finalized 3-factor VISS structure (paper @tbl-viss). aims.1 loads
## negatively on cynicism (reverse-coded item).
pop_spec = '
  textbook    =~ consensus.2 + fallible.1 + pluralism.1
  cynicism    =~ coi.1 + coi.2 + consensus.1 + aims.1
  objectivity =~ nonsubj.1 + nonsubj.2 + vfi.1
'

viss_df = here(data_dir, '01_data.Rds') |>
  read_rds() |>
  select(starts_with('viss')) |>
  rename_with(~ str_remove(.x, 'viss.')) |>
  ## keep only the items in the 3-factor model
  select(
    consensus.2, fallible.1, pluralism.1,
    coi.1, coi.2, consensus.1, aims.1,
    nonsubj.1, nonsubj.2, vfi.1
  )

## Default marker-variable scaling: orientation fixed, no cross-rep sign-flips.
pop_fit = cfa(pop_spec, data = viss_df)
assert_that(lavInspect(pop_fit, 'converged'),
            msg = 'Population CFA on Study 3 data did not converge.')

#' Build lavaan syntax with population parameter values baked in as fixed
#' coefficients, so simulateData() draws from the Study 3 estimates.
#'
#' @param fit a fitted lavaan object
#' @return a length-1 character vector of lavaan model syntax
build_pop_syntax = function(fit) {
  pe = parameterEstimates(fit)
  loadings = pe |>
    filter(op == '=~') |>
    transmute(line = glue('{lhs} =~ {round(est, 4)}*{rhs}'))
  vars = pe |>
    filter(op == '~~') |>
    transmute(line = glue('{lhs} ~~ {round(est, 4)}*{rhs}'))
  str_c(c(loadings$line, vars$line), collapse = '\n')
}

pop_syntax = build_pop_syntax(pop_fit)
write_lines(pop_syntax, here(out_dir, '01_pop_syntax.txt'))

## Population values + the parameters we care about ----
## "Parameters of interest" = freely estimated loadings (markers are fixed) and
## the three factor covariances. These bind power hardest.
pop_values = parameterEstimates(pop_fit) |>
  filter(op %in% c('=~', '~~')) |>
  transmute(
    param = as.character(glue('{lhs}{op}{rhs}')),
    lhs, op, rhs,
    pop = est,
    is_free = se > 0,                       # markers have se == 0 (fixed)
    type = case_when(
      op == '=~' ~ 'loading',
      op == '~~' & lhs == rhs & lhs %in% c('textbook', 'cynicism', 'objectivity') ~ 'factor variance',
      op == '~~' & lhs == rhs ~ 'residual variance',
      op == '~~' & lhs != rhs ~ 'factor covariance'
    ),
    of_interest = is_free & type %in% c('loading', 'factor covariance')
  )

interest_params = pop_values |>
  filter(of_interest) |>
  pull(param)

## Monte Carlo engine ----

#' Simulate one dataset of size n from the population model, refit, and return
#' per-parameter results plus convergence / admissibility flags.
#'
#' @param n integer sample size
#' @return a tibble, one row per model parameter
sim_once = function(n) {
  dat = simulateData(pop_syntax, sample.nobs = n)
  fit = tryCatch(
    cfa(pop_spec, data = dat),
    error = function(e) NULL,
    warning = function(w) suppressWarnings(cfa(pop_spec, data = dat))
  )

  if (is.null(fit) || !lavInspect(fit, 'converged')) {
    return(tibble(param = NA_character_, converged = FALSE, admissible = FALSE))
  }

  pe = parameterEstimates(fit)
  ## admissible = no negative variances (Heywood case)
  admissible = pe |>
    filter(op == '~~', lhs == rhs) |>
    pull(est) |>
    (\(v) all(v > 0))()

  pe |>
    filter(op %in% c('=~', '~~')) |>
    transmute(
      param = as.character(glue('{lhs}{op}{rhs}')),
      est, se, pvalue, ci.lower, ci.upper,
      converged = TRUE,
      admissible = admissible
    )
}

## Run across the grid (parallel, with a sequential fallback) ----
## Every (n, rep) is flattened into one job list and mapped at once, so workers
## stay busy across sample sizes rather than idling between them. We probe
## whether parallel workers can actually launch (some sandboxed sessions block
## the sockets multisession needs) and fall back to sequential if not. Because
## furrr_options(seed = TRUE) draws parallel-safe L'Ecuyer streams from the
## set.seed() below, the results are reproducible *and identical* whether this
## runs in parallel or sequentially.
cli_h1('Monte Carlo CFA power analysis')

jobs = expand_grid(n = n_grid, rep = seq_len(n_reps))

n_workers = max(1, availableCores() - 1)
can_parallel = tryCatch(
  {
    plan(multisession, workers = n_workers)
    identical(value(future(TRUE)), TRUE)   # force a worker to actually run
  },
  error = function(e) FALSE
)
if (!can_parallel) {
  plan(sequential)
  n_workers = 1L
  cli_alert_warning('Parallel workers unavailable; running sequentially.')
}
on.exit(plan(sequential), add = TRUE)
cli_alert_info('Running {nrow(jobs)} simulations on {n_workers} worker(s)')

set.seed(base_seed)
raw = future_map2(
  jobs$n, jobs$rep,
  ~ sim_once(.x) |> mutate(n = .x, rep = .y),
  .options = furrr_options(
    seed = TRUE,
    packages = c('lavaan', 'dplyr', 'tibble', 'stringr', 'glue', 'tidyr'),
    globals = c('sim_once', 'pop_spec', 'pop_syntax')
  ),
  .progress = TRUE
) |>
  list_rbind()

plan(sequential)

## Summarize ----
## Per-rep convergence/admissibility (one flag per replication)
rep_flags = raw |>
  distinct(n, rep, converged, admissible)

conv_summary = rep_flags |>
  group_by(n) |>
  summarize(
    conv_rate = mean(converged),
    admiss_rate = mean(converged & admissible),
    .groups = 'drop'
  )

## Per-parameter summary, using only converged + admissible reps
power_summary = raw |>
  filter(converged, admissible, !is.na(param)) |>
  inner_join(pop_values, by = 'param') |>
  group_by(n, param, type, of_interest, pop) |>
  summarize(
    mean_est = mean(est),
    emp_se = sd(est),                                  # empirical SE = SD of estimates
    mean_se = mean(se),                                # average estimated SE
    bias_pct = 100 * (mean(est) - first(pop)) / first(pop),
    se_bias_pct = 100 * (mean(se) - sd(est)) / sd(est),
    coverage = mean(ci.lower <= first(pop) & first(pop) <= ci.upper),
    power = mean(pvalue < 0.05),
    .groups = 'drop'
  ) |>
  left_join(conv_summary, by = 'n') |>
  arrange(n, param)

write_rds(power_summary, here(out_dir, '01_power_summary.Rds'))
write_csv(power_summary, here(out_dir, '01_power_summary.csv'))

## Recommended N ----
## Smallest n meeting all acceptance criteria for the parameters of interest.
criteria_by_n = power_summary |>
  filter(of_interest) |>
  group_by(n) |>
  summarize(
    conv_ok = first(admiss_rate) >= 0.95,
    bias_ok = all(abs(bias_pct) <= 10),
    se_bias_ok = all(abs(se_bias_pct) <= 10),
    coverage_ok = all(coverage >= 0.91 & coverage <= 0.98),
    power_ok = all(power >= 0.80),
    min_power = min(power),
    .groups = 'drop'
  ) |>
  mutate(all_ok = conv_ok & bias_ok & se_bias_ok & coverage_ok & power_ok)

recommended_n = criteria_by_n |>
  filter(all_ok) |>
  slice_min(n, n = 1) |>
  pull(n)

recommended_n = if (length(recommended_n) == 0) NA_integer_ else recommended_n

## Power curve figure ----
power_plot = power_summary |>
  filter(of_interest) |>
  mutate(label = str_replace(param, '=~', ': ') |> str_replace('~~', ' <-> ')) |>
  ggplot(aes(n, power, color = type, group = param)) +
  geom_hline(yintercept = 0.80, linetype = 'dashed') +
  geom_line(alpha = 0.8) +
  geom_point(size = 1) +
  scale_color_manual(values = c('loading' = '#2166ac',
                                'factor covariance' = '#d73027')) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = n_grid) +
  labs(
    x = 'Sample size (n)',
    y = 'Power (P[parameter significant at .05])',
    color = NULL,
    title = 'CFA power by sample size, finalized 3-factor VISS model',
    subtitle = 'Each line is one freely estimated parameter; dashed line is .80'
  ) +
  theme(legend.position = 'bottom')

ggsave(here(out_dir, '01_power_curve.png'), power_plot,
       width = 7, height = 5, dpi = 250, bg = 'white')

## Report ----
cli_h1('Results')
print(criteria_by_n)
if (is.na(recommended_n)) {
  cli_alert_warning(
    'No sample size in the grid met all criteria. Extend n_grid upward.'
  )
} else {
  binding = power_summary |>
    filter(n == recommended_n, of_interest) |>
    slice_min(power, n = 3) |>
    transmute(param, power = round(power, 3))
  cli_alert_success('Recommended Study 4 N = {recommended_n}')
  cli_alert_info('Binding (lowest-power) parameters at n = {recommended_n}:')
  print(binding)
}
