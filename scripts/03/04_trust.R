library(tidyverse)
theme_set(theme_bw())
library(sjlabelled)
library(ggforce)
library(ggpubr)
library(ggeffects)

library(here)

source(here('R', 'vars.R'))

data_dir = here('data', '03')

viss_scores = read_rds(here(data_dir, '03_scores_3.Rds')) |> 
    magrittr::set_colnames(c('prolific_id', 
                             'viss_cynicism', 
                             'viss_textbook', 
                             'viss_objectivity'))

dataf = read_rds(here(data_dir, '01_data.Rds')) |> 
    left_join(viss_scores, by = 'prolific_id')


    
## Trust measures EDA ----
## CoSS ----
dataf |> 
    select(starts_with('coss.')) |> 
    cor(use = 'pairwise.complete.obs')
ggplot(dataf) +
    geom_point(aes(x = .panel_x, y = .panel_y), 
               position = 'jitter', alpha = .1) +
    stat_smooth(aes(.panel_x, .panel_y), 
                method = 'lm') +
    facet_matrix(vars(starts_with('coss')))
## Alpha > .9
dataf |> 
    select(starts_with('coss.')) |> 
    psych::alpha(check.keys = TRUE)

ggplot(dataf, aes(coss)) +
    geom_density()

## GSS ----
dataf |> 
    select(prolific_id, starts_with('gss')) |> 
    mutate(across(-prolific_id, as_label)) |> 
    pivot_longer(-prolific_id, 
                 names_to = 'institution', 
                 values_to = 'confidence') |> 
    ggplot(aes(institution, 
               fill = confidence)) +
    geom_bar(color = 'black', size = .1) +
    scale_fill_brewer(palette = 'RdBu') +
    coord_flip()

## Pew "effect of science on society" ----
ggplot(dataf, aes(as_label(effect))) +
    geom_bar()


## Scientism ----
## alpha around .80
dataf |> 
    select(starts_with('scientism.')) |> 
    psych::alpha()

## For most items, plurality selected "critical trust"
## V. consistent scientism for `problems`; 
## V. consistent "antiscience" for `critique`
dataf |> 
    select(starts_with('scientism.')) |> 
    mutate(across(
        .fns = ~ factor(.x,
                        levels = 1:5,
                        labels = c('Extremely antiscientific', 
                                   'Moderately antiscientific', 
                                   'Critical trust', 
                                   'Moderately scientistic', 
                                   'Extremely scientistic')))) |> 
    pivot_longer(cols = everything(), 
                 names_to = 'item', 
                 values_to = 'response') |> 
    mutate(item = str_remove(item, 'scientism.')) |> 
    ggplot(aes(item, fill = response)) +
    geom_bar(color = 'black', size = .1) +
    scale_fill_brewer(palette = 'RdBu') +
    coord_flip()

ggplot(dataf, aes(scientism)) +
    geom_density() +
    geom_rug()

## Correlations among trust measures ----
## Generally moderate 
dataf |> 
    select(!!trust_vars) |> 
    cor(use = 'pairwise.complete.obs')

## No clue why the scales aren't adjusting to accommodate density plots
ggplot(dataf) +
    geom_autopoint(alpha = .1, position = 'jitter') +
    stat_smooth(aes(.panel_x, .panel_y), method = 'lm') +
    geom_autodensity(color = 'black', fill = 'transparent') +
    facet_matrix(vars(!!trust_vars), 
                 layer.diag = 3)

## TODO: Is Cronbach's alpha even a coherent thing here? 
dataf |> 
    select(!!trust_vars) |> 
    psych::alpha()


## Demographics against trust measures ----
## Clear negative corr btwn right-wing measures and trust measures
## Prestige and OSI generally weakly positive
ggplot(dataf) +
    geom_autopoint(alpha = .1) +
    stat_smooth(aes(.panel_x, .panel_y),
                method = 'lm', na.rm = TRUE) +
    facet_matrix(rows = vars(!!trust_vars), 
                 cols = vars(!!demo_cont))


## ViSS items against trust measures ----
## These are very hard to read; probably better to use EFA factors
viss_items = dataf |>
    names() |>
    keep(~ str_detect(.x, 'viss'))
ggplot(dataf) +
    geom_autopoint(alpha = .1, position = 'jitter') +
    stat_smooth(aes(.panel_x, .panel_y), method = 'lm') +
    facet_matrix(rows = vars(!!trust_vars),
                 cols = vars(viss_items[1:10]))
ggplot(dataf) +
    geom_autopoint(alpha = .1, position = 'jitter') +
    stat_smooth(aes(.panel_x, .panel_y), method = 'lm') +
    facet_matrix(rows = vars(!!trust_vars),
                 cols = vars(viss_items[11:19]))

## VISS factors against trust measures ----
ggplot(dataf) +
    geom_autopoint(alpha = .1, position = 'jitter') +
    stat_smooth(aes(.panel_x, .panel_y), method = 'lm') +
    stat_cor(aes(.panel_x, .panel_y,
                 label = after_stat(r.label)), 
             geom = 'label',
             label.y = 2, 
             digits = 1) +
    facet_matrix(rows = vars(!!trust_vars),
                 cols = vars(!!viss_vars))

## Regressions ----
## NB due to high missingness, exclude `occ_prestige`
lm(coss ~ gender + age + religious + rwa.conservatism + rwa.traditionalism + rwa.authoritarianism + osi_score + viss_cynicism + viss_textbook + viss_objectivity, 
   data = dataf) |> 
    summary()


## Focusing on RWA ----
## Scatterplots suggest more conservative -> lower trust
ggplot(dataf) +
    geom_autopoint(alpha = .2) +
    stat_smooth(aes(.panel_x, .panel_y),
                method = 'lm', na.rm = TRUE) +
    facet_matrix(rows = vars(coss), 
                 cols = vars(c(rwa.conservatism, 
                               rwa.traditionalism, 
                               rwa.authoritarianism, 
                               rwa)))

## But separate the 3 dimensions of conservatism
rwa_fit = lm(coss ~ rwa.conservatism + rwa.traditionalism + 
                 rwa.authoritarianism, 
             data = dataf)
summary(rwa_fit)

## Marginal effects plot shows conservatism (deference to establishment) is *positively* correlated w/ trust
## Traditionalism ("social conservatism," incld religion) has largest negative correlation 
lst('rwa.conservatism', 
    'rwa.traditionalism', 
    'rwa.authoritarianism') |> 
    map(~ predict_response(rwa_fit, 
                           terms = .x, 
                           margin = 'empirical')) |> 
    bind_rows(.id = 'focal_var') |> 
    ggplot(aes(x, predicted)) + 
    geom_ribbon(aes(ymin = conf.low, 
                    ymax = conf.high), 
                alpha = .4) +
    geom_line() +
    facet_wrap(vars(focal_var)) +
    xlab('') +
    ylim(1, 7)

