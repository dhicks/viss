# Study 4 sample-size justification — prose for the manuscript

This file holds two paste-ready write-ups of the CFA sample-size analysis
(`01_cfa_power_sim.R`, cross-checked by `02_rules_of_thumb.R`):

1. an **accessible** version for the main text (audience: philosophers of science); and
2. a **technical** version for the supplemental methods (audience: methods reviewers).

The headline result: a Study 4 of **N = 400** is adequate to confirm the finalized
3-factor VISS structure by CFA; we recommend recruiting **~450** to leave a safety margin.

The manuscript text for each section is between the `--- begin/end` markers, ready to paste
into `paper.qmd` with no editing of the prose itself (just check the citation keys).

---

## 1. Accessible version (main paper)

--- begin main-text paragraph ---

A confirmatory factor analysis (CFA) tests a *pre-specified* factor structure against fresh
data, and it needs enough respondents to estimate that structure reliably. In Study 3 we
could not run a CFA: doing both an exploratory and a confirmatory analysis on one dataset
requires splitting the sample — discovering the structure in one half and confirming it in
the other — and splitting Study 3's ~500 respondents left each half too small to analyze
well. Study 4 avoids this problem, because the factor structure is now fixed in advance; the
entire Study 4 sample can be devoted to the CFA.

To determine how large that sample must be, we ran a simulation study [@MuthenHowUse2002].
Using the factor structure and parameter estimates from Study 3 as a stand-in for the truth,
we repeatedly generated artificial datasets of different sizes, re-ran the CFA on each, and
recorded how often the analysis recovered the underlying structure accurately and with
adequate statistical power. The analysis becomes reliable — converging on admissible
solutions, recovering the loadings and factor correlations with little bias, and detecting
them with at least 80% power — at about **400 respondents**. The hardest quantities to pin
down are the correlations among the three factors, which are modest in size and so require
the larger sample. This simulation-based target is consistent with, and slightly above,
conventional rules of thumb for CFA sample size (which point to roughly 200–460 respondents
for a model of this size). We therefore aimed to recruit approximately 450 respondents for
Study 4.

--- end main-text paragraph ---

*Citation needed in `VISS.yaml`:* Muthén & Muthén (2002), "How to use a Monte Carlo study
to decide on sample size and determine power," *Structural Equation Modeling* 9(4): 599–620.

---

## 2. Technical version (supplement)

--- begin supplement subsection ---

**Sample-size determination for the Study 4 CFA.** Because the finalized VISS factor
structure was fixed by the Study 3 exploratory analysis, Study 4 is purely confirmatory and
the full sample is available for the CFA (no exploratory/confirmatory split is required). We
determined the target sample size by a Monte Carlo simulation following @MuthenHowUse2002,
implemented in R with `lavaan` [version 0.6-20\; @RosseelLavaanLatentVariable2024].

*Population model.* We took the finalized three-factor model — *textbook* (consensus.2,
fallible.1, pluralism.1), *cynicism* (coi.1, coi.2, consensus.1, and the reverse-keyed
aims.1), and *objectivity* (nonsubj.1, nonsubj.2, vfi.1) — and fit it to the full Study 3
sample by maximum likelihood, treating the seven-point items as continuous (as in the Study
3 analysis). The resulting estimates served as population values for the simulation.
Standardized factor loadings ranged from 0.31 to 0.86, and the three inter-factor
correlations were modest (0.23, 0.27, and 0.23).

*Procedure.* For each candidate sample size in {150, 200, 250, 300, 350, 400, 450, 500}, we
drew 1,000 datasets from the population model (`lavaan::simulateData`, multivariate normal)
and refit the model to each. For every replication we recorded convergence, solution
admissibility (no negative variance estimates), and the estimates, standard errors,
*p*-values, and 95% confidence intervals for each free parameter. Following
@MuthenHowUse2002, we treated a sample size as adequate when, for the freely estimated
loadings and the three factor correlations: the rate of convergent, admissible solutions was
at least 0.95; absolute parameter bias and standard-error bias were at most 10%; 95%
confidence-interval coverage fell in [0.91, 0.98]; and power (the probability of a parameter
being significant at α = .05) was at least 0.80.

*Results.* All criteria were first satisfied at N = 400, where the rate of convergent,
admissible solutions was 0.998 and minimum power across the parameters of interest was 0.83.
The binding parameters were the inter-factor correlations — in particular the
cynicism–objectivity correlation (power 0.83 at N = 400) — which is expected given their
modest magnitude. At smaller samples the model frequently produced inadmissible solutions
(e.g., the admissibility rate was 0.88 at N = 150 and 0.94 at N = 200), and power for the
factor correlations was below 0.80 (e.g., 0.70 for the binding parameter at N = 300). At the
largest sample examined (N = 500), parameter bias was at most 3.3% and coverage ranged from
0.93 to 0.95, confirming that the data-generating and fitted models agreed. These simulation
results are consistent with, and slightly above, conventional rules of thumb (Table S\ref),
which place the minimum sample size for a model with 23 free parameters and ten indicators at
roughly 200–460; the upper end is appropriate here because several VISS items have low
communalities [@MacCallumSampleSizeFactor1999]. We therefore set a recruitment target of
approximately 450 respondents for Study 4, leaving a margin above the 400-respondent minimum.

--- end supplement subsection ---

*Citations to confirm/add in `VISS.yaml`:* Muthén & Muthén (2002); Rosseel `lavaan` (already
cited as `@RosseelLavaanLatentVariable2024`); MacCallum, Widaman, Zhang & Hong (1999),
"Sample size in factor analysis," *Psychological Methods* 4(1): 84–99. The rules-of-thumb
table (`out/04/02_rules_of_thumb.html`) also cites Boomsma (1985), Bentler & Chou (1987),
Jackson (2003), and Kline (2016) — add these if the table is included.

*Figure for the supplement:* `out/04/01_power_curve.png` (power vs. N for each free
parameter, with the 0.80 reference line). Stage it into `paper/img/` if it goes in the paper.
