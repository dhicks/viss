library(tidyverse)
theme_set(theme_bw())
library(ggforce)
library(ggpubr)
library(broom)
library(sjlabelled)
library(gt)

library(visdat)
library(psych)
library(lavaan)

library(here)
library(glue)
library(assertthat)

source(here('R', 'highlight_cells.R'))

data_dir = here('data', '03')
out_dir = here('out', '03')

## Load data ----
## NB KMO and Lavaan errors indicate dataset is too small for EFA/CFA split
set.seed(2024-04-19)
dataf = here(data_dir, '01_data.Rds') |> 
    read_rds()

viss_df = dataf |> 
    select(prolific_id, starts_with('viss')) |> 
    rename_with(~ str_remove(.x, 'viss.')) |> 
    column_to_rownames('prolific_id')

set.seed(2024-07-11)
datafs = viss_df |> 
    mutate(split = rbinom(n(), 1, 0.5)) %>%
    split(.$split) |> 
    map(~ select(.x, !split))

efa_df = datafs[[1]]
cfa_df = datafs[[2]]

## Hierarchical clustering ----
vis_cor(viss_df) +
    scale_fill_distiller(palette = 'RdBu', 
                         direction = 1, limits = c(-1, 1))

hclust_fit = cor(viss_df, use = 'pairwise.complete.obs') |> 
    as.dist() |> 
    hclust()
plot(hclust_fit)

## Check EFA assumptions ----
## Correlation matrix sig. diff. from identity
bartlett.test(efa_df)
bartlett.test(viss_df)

## Determinant / multicollinearity
efa_df |> 
    as.matrix() |> 
    cor(use = 'pairwise.complete.obs') |> 
    det()

## Sample size
## With the split, several are "mediocre" and value.conflict is "unacceptable"
KMO(efa_df)
## Generally better with the whole dataset, though aims.2, stdpt, value.conflict, vfi.1 still below 0.7
KMO(viss_df)

## Fit EFA ----
efa_parallel = fa.parallel(viss_df, fm = "minres", fa = "fa")
efa_parallel

n_factors = 1:6
efa_fits = map(n_factors, 
               ~ fa(viss_df, nfactors = .x, rotate = 'varimax'))

communalities = map(efa_fits, ~ {.x |> 
        loadings() %>%
        {.^2} |> 
        rowSums() %>%
        {1 - .}})

## Total variance explained
efa_fits[[1]]$Vaccounted
map_dbl(efa_fits[2:6], 
    ~ .$Vaccounted %>%
        .['Cumulative Var',] |> 
        max()) |> 
    print(digits = 2)

write_rds(efa_fits, here(data_dir, '04_efa_fits.Rds'))

## Inspect loadings ----
## Lables for the latent variables
## NB follow the order presented, not numbered
efa_labels = list()

loadings(efa_fits[[1]]) |> 
    print(cutoff = .3)
efa_labels[[1]] = c('MR1_1' = 'textbook + cynicism_1')

## 2 sort of looks like textbook and cynicism
loadings(efa_fits[[2]]) |> 
    print(cutoff = .3)
efa_labels[[2]] = c('MR1_2' = 'textbook_2', 'MR2_2' = 'cynicism_2')

## 3: (2) cynicism; (1) textbook; objectivity
## 3 seems more conceptually coherent than 4
loadings(efa_fits[[3]]) |> 
    print(cutoff = .3)
efa_labels[[3]] = c('MR2_3' = 'cynicism_3', 'MR1_3' = 'textbook_3', 'MR3_3' ='objectivity_3')

## 4: textbook; cynicism?; (4) objectivity; (3) ??? (aims + stdpt)
loadings(efa_fits[[4]]) |> 
    print(cutoff = .3)
efa_labels[[4]] = c('MR1_4' = 'textbook + aims.3_4', 'MR2_4' = 'cynicism_4', 
                    'MR4_4' = 'objectivity_4', 'MR3_4' = '?(aims + stdpt)_4')

## 5: (2) cynicism; (1) textbook + aims.3; (4) objectivity; (3) ??? (aims + stdpt); (5) pluralism.3
loadings(efa_fits[[5]]) |> 
    print(cutoff = .3)
efa_labels[[5]] = c('MR2_5' = 'cynicism_5', 'MR1_5' = 'textbook + aims.3_5', 
                    'MR4_5' = 'objectivity_5', 'MR3_5' = '?(aims + stdpt)_5', 
                    'MR5_5' = 'pluralism.3_5')

## 6: (2) cynicism; (1) textbook; (4) objectivity; (3) ??? (aims + stdpt); (5) pluralism.3; (6) ir
loadings(efa_fits[[6]]) |> 
    print(cutoff = .3)
efa_labels[[6]] = c('MR2_6' = 'cynicism_6', 'MR1_6' = 'textbook_6', 
                    'MR4_6' = 'objectivity_6', 'MR3_6' = '?(aims + stdpt)_6',
                    'MR5_6' = 'pluralism.3_6', 'MR6_6' = 'ir_6')

efa_labels_df = efa_labels[1:6] |> 
    map(enframe, name = 'term', value = 'label') |> 
    bind_rows()


## Scores ----
## Fitted EDAs include a `scores` element with the right dimensions
## But since default for `factanal` is `scores = 'none'` it's not clear what this means
efa_fits[[3]] %>%
    {.$scores} |> 
    str()

## Discretized / "non-refined" scores
scores = function(this_efa, efa_name = 'test', threshold = 0.3) {
    load_mx = this_efa |> 
        loadings() |> 
        magrittr::set_class('matrix') %>%
        {(abs(.) > threshold) * (sign(.))}
    
    dataf_mx = as.matrix(viss_df)
    
    assert_that(all(rownames(load_mx) == colnames(dataf_mx)))
    
    dataf_mx %*% load_mx |> 
        as_tibble(rownames = 'prolific_id') |> 
        rename_with(.cols = -prolific_id, 
                    .fn = ~ glue('{.x}_{efa_name}'))
}

efa_scores = imap(efa_fits, scores) |> 
    imap(~ write_rds(.x, here(data_dir, 
                              glue('04_scores_{.y}.Rds'))))

## Big combined loadings table ----
loadings_to_df = function(fit) {
    fit |> 
        loadings() |> 
        magrittr::set_class('matrix') |> 
        as_tibble(rownames = 'variable')
}
loadings_df = efa_fits |> 
    map(loadings_to_df) |> 
    imap(\(x, y) rename_with(x, 
                             .cols = !variable,
                             .fn = ~ glue('{.x}_{y}'))) |> 
    reduce(~ full_join(.x, .y, by = 'variable'))

loadings_gt = loadings_df |> 
    pivot_longer(!variable, names_to = 'latent', values_to = 'value') |> 
    # filter(abs(value) > .3) |>
    inner_join(efa_labels_df, by = c('latent' = 'term')) |> 
    select(!latent) |> 
    pivot_wider(names_from = 'label', values_fro = 'value', 
                names_sort = TRUE) |> 
    relocate(contains('aims.3'), .after = textbook_3) |> 
    gt() |> 
    fmt_number(columns = !variable,
               decimals = 2) |>
    highlight_cells() |> 
    highlight_cells(cell_style = cell_fill('#F0F0F0')) |> 
    sub_missing(missing_text = '') |> 
    tab_spanner_delim(delim = '_') |> 
    tab_style(
        style = cell_borders(
            sides = c('left'),
            # weight = px(.5)
        ),
        locations = cells_body(columns = c(2, 5, 10, 11, 15, 17)))
loadings_gt

gtsave(loadings_gt, here(out_dir, '04_loadings.html'))
gtsave(loadings_gt, here(out_dir, '04_loadings.pdf'))
write_rds(loadings_gt, here(out_dir, '04_loadings.Rds'))

## Scores and demographics ----
## TODO: move to separate script
demo_cont = expr(c(age, religious, politics, 
                   rwa.conservatism, rwa.traditionalism, 
                   rwa.authoritarianism, rwa, 
                   occ_prestige, osi_score))

demo_cat = expr(c(gender, race_ethnicity, party))

scores_df = dataf |> 
    left_join(efa_scores[[3]], by = 'prolific_id') |> 
    rename(cynicism = MR2_3, 
           textbook = MR1_3, 
           objectivity = MR3_3)

ggplot(scores_df) +
    geom_autopoint(alpha = .1) +
    stat_smooth(aes(.panel_x, .panel_y),
                method = 'lm', na.rm = TRUE) +
    stat_cor(aes(.panel_x, .panel_y,
                 label = after_stat(r.label)), 
             geom = 'label',
             label.y = 2, 
             digits = 1) +
    facet_matrix(rows = vars(cynicism, textbook, objectivity), 
                 cols = vars(!!demo_cont))

scores_df |> 
    ggplot(aes(x = .panel_x, y = .panel_y, group = .panel_y)) +
    geom_point(position = position_jitter(width = 1, height = 0),
               alpha = .1) +
    stat_summary(color = 'red',
                 fun.data = mean_cl_boot,
                 geom = 'pointrange', shape = '|', fatten = 10) +
    facet_matrix(rows = vars(!!demo_cat), 
                 cols = vars(cynicism, textbook, objectivity))

lm_fit = lm(cynicism ~ gender + race_ethnicity + age + religious + politics + rwa.authoritarianism + rwa.conservatism + rwa.traditionalism + osi_score, 
   data = scores_df)

summary(lm_fit)

tidy(lm_fit) |> 
    mutate(ci.lo = estimate + qnorm(.025) * std.error, 
           ci.hi = estimate + qnorm(.975) * std.error) |> 
    filter(!str_detect(term, 'race_ethnicity'), 
           !str_detect(term, 'gender'), 
           !str_detect(term, '(Intercept)')) |> 
    ggplot(aes(term, estimate, ymin = ci.lo, ymax = ci.hi)) +
    geom_pointrange() +
    geom_hline(yintercept = 0) +
    # facet_wrap(vars(response)) +
    coord_flip()

## Regress COSS against each latent variable and extract coefficients ----
coss_coefs = efa_scores[1:6] |> 
    map(~ {.x |> 
            inner_join(dataf, by = 'prolific_id') |> 
            select(starts_with('MR'), coss) %>%
            lm(coss ~ ., data = .) |> 
            tidy(conf.int = TRUE)}) |> 
    map(~ filter(.x, term != '(Intercept)')) |> 
    bind_rows() |> 
    left_join(efa_labels_df, by = 'term')

## cynicism is consistently negative, ~ -0.18
## pluralism and (aims + stdpt) consitently weak positive, ~ 0.09
## others all negligible
arrange(coss_coefs, label)

ggplot(coss_coefs, aes(fct_rev(label), estimate)) +
    geom_hline(yintercept = c(0, -1/7, 1/7),
               linetype = 'dashed') +
    geom_pointrange(aes(ymin = conf.low, 
                        ymax = conf.high)) +
    coord_flip() +
    labs(x = 'factor', y = 'regression coefficient')
    


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
    cfa_fit = cfa(lavaan_spec, data = viss_df)
    
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
## FWIW 3 also just barely has the best fit statistics
## Common thresholds: 
## CFI: >=0.95 (x - way below)
## AGFI: >=0.9 (✓)
## RMSEA: <=0.06 (x - a bit above)
## SRMR: <= 0.08 (✓)
map(efa_fits, check_cfa) |> 
    bind_rows(.id = 'n_factors')


