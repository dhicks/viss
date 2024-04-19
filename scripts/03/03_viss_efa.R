library(tidyverse)
theme_set(theme_bw())
library(sjlabelled)

library(psych)
library(lavaan)

library(here)
library(glue)

data_dir = here('data', '03')

## Load data ----
## TODO: check w/ Emilio if sample is large enough for this split
set.seed(2024-04-19)
datafs = here(data_dir, '01_data.Rds') |> 
    read_rds() |> 
    select(prolific_id, starts_with('viss')) |> 
    rename_with(~ str_remove(.x, 'viss.')) |> 
    mutate(split = rbinom(n(), 1, 0.5)) |> 
    group_by(split) |> 
    group_split() |> 
    map(~ select(.x, !split)) |> 
    map(~ column_to_rownames(.x, 'prolific_id'))
dataf = bind_rows(datafs)

efa_df = datafs[[1]]
efa_mx = efa_df |> 
    as.matrix()
cfa_df = datafs[[2]]

## Hierarchical clustering ----
visdat::vis_cor(dataf) +
    scale_fill_distiller(palette = 'RdBu', 
                         direction = 1, limits = c(-1, 1))

hclust_fit = cor(dataf, use = 'pairwise.complete.obs') |> 
    as.dist() |> 
    hclust()
plot(hclust_fit)

## Check EFA assumptions ----
## Correlation matrix sig. diff. from identity
bartlett.test(dataf)

## Determinant / multicollinearity
efa_mx |> 
    cor(use = 'pairwise.complete.obs') |> 
    det()

## Sample size
## With the split, several are "mediocre" and value.conflict is "unacceptable"? 
KMO(efa_df)
## Generally better with the whole dataset, though aims.2, stdpt, value.conflict, vfi.1 still below 0.7
KMO(dataf)

## Fit EFA ----
efa_parallel = fa.parallel(efa_df, fm = "minres", fa = "fa")
efa_parallel

n_factors = 1:6
efa_fits = map(n_factors, 
               ~ fa(efa_df, nfactors = .x, rotate = 'varimax'))

communalities = map(efa_fits, ~ {.x |> 
        loadings() %>%
        {.^2} |> 
        rowSums() %>%
        {1 - .}})


## Inspect loadings ----
## 2 sort of looks like unity of science and cynicism
loadings(efa_fits[[2]]) |> 
    print(cutoff = .3)

## 3: cynicism; unity of science; ??
loadings(efa_fits[[3]]) |> 
    print(cutoff = .3)

## 4: unity of science; cynicism; objectivity; ??? (aims + stdpt)
loadings(efa_fits[[4]]) |> 
    print(cutoff = .3)

## 5: (2) cynicism; (1) unity of science; (4) objectivity; (3) ??? (aims + stdpt); (5) ??? (ir + no imagination)
loadings(efa_fits[[5]]) |> 
    print(cutoff = .3)

## 6: cynicism; fixity; objectivity; experiments; aims; ??? (ir + no imagination)
loadings(efa_fits[[6]]) |> 
    print(cutoff = .3)

## CFA ----
check_cfa = function(this_efa, threshold = 0.3) {
    factors = colnames(loadings(this_efa))
    obs_vars = rownames(loadings(this_efa))
    
    ## Discretize loadings
    load_mx = loadings(this_efa) |> 
        magrittr::set_class('matrix') %>% 
        {(abs(.) > threshold) * (sign(.))}
    
    ## Split into columns (factors) and build lavaan spec
    lavaan_spec = load_mx |> 
        asplit(2) |> 
        map(~ .x[.x != 0]) |> 
        map(names) |> 
        map(~ str_c(.x, collapse = ' + ')) |> 
        imap(~ glue('{.y} =~ {.x}')) |> 
        str_c(collapse = '\n')
    
    ## Fit CFA
    ## NB optimizer doesn't converge w/ split data
    cfa_fit = cfa(lavaan_spec, data = dataf)
    
    if (!cfa_fit@Fit@converged) {
        return(data.frame(chisq = NA))
    }
    # summary(cfa_fit, fit.measures = TRUE)
    fitmeasures(cfa_fit, 
                c('chisq', 'cfi', 'agfi', 
                  'rmsea', 'rmsea.ci.upper', 
                  'srmr')) |> 
        as_tibble_row() |> 
        mutate(across(.fns = as.numeric))
}

# debugonce(check_cfa)
# check_cfa(efa_fits[[5]]) |> str()
map(efa_fits, check_cfa) |> 
    bind_rows(.id = 'n_factors')


