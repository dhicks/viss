library(tidyverse)
theme_set(theme_bw())
library(sjlabelled)
library(ggforce)
library(ggpubr)
library(ggeffects)
library(gtsummary)
library(gt)
library(corrr)

library(patchwork)


library(here)

source(here('R', 'vars.R'))

data_dir = here('data', '03')
out_dir = here('out', '03')

viss_scores = read_rds(here(data_dir, '04_scores_3.Rds')) |> 
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
corr_gt = dataf |> 
    select(!!trust_vars) |> 
    # cor(use = 'pairwise.complete.obs')
    correlate() |> 
    gt() |> 
    fmt_number(decimals = 2) |> 
    fmt_missing(missing_text = '')
corr_gt

## No clue why the scales aren't adjusting to accommodate density plots
corr_gg = ggplot(dataf) +
    geom_autopoint(alpha = .1, position = 'jitter') +
    stat_smooth(aes(.panel_x, .panel_y), method = 'lm') +
    geom_autodensity(color = 'black', fill = 'transparent') +
    facet_matrix(vars(!!trust_vars), 
                 layer.diag = 3)
corr_gg

corr_gg + as_gtable(corr_gt) +
    plot_layout(ncol = 1, 
                heights = c(3, 1))

ggsave(here(out_dir, '05_trust_corr.png'), 
       height = 6, width = 6, bg = 'white', scale = 1.25)

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
## TODO: x-axes don't quite align
viss_coss = ggplot(dataf) +
    geom_autopoint(alpha = .2, position = 'jitter') +
    stat_smooth(aes(.panel_x, .panel_y), method = 'lm') +
    stat_cor(aes(.panel_x, .panel_y,
                 label = after_stat(label)), 
             geom = 'label',
             label.y = 1, 
             digits = 1) +
    facet_matrix(rows = vars(coss),
                 cols = vars(!!viss_vars))
viss_coss

coss_hist = dataf |> 
    ggplot(aes(coss)) +
    geom_histogram(binwidth = .25) +
    coord_flip() +
    labs(x = '')

viss_hist = dataf |> 
    select(!!viss_vars) |> 
    pivot_longer(everything(), 
                 names_to = 'item', 
                 values_to = 'response', 
                 names_transform = list(item = fct_inorder)) |> 
    ggplot(aes(response)) +
    geom_histogram(binwidth = .5) +
    labs(x = '') +
    facet_grid(cols = vars(item), 
               scales = 'free')

viss_hist + viss_coss + coss_hist +
    plot_layout(design = 'AAAAD
                          BBBBC
                          BBBBC', 
                axes = 'collect') +
    labs(caption = 'N = 502')

ggsave(here(out_dir, '05_factors_coss.png'), 
       height = 3, width = 5, scale = 2,
       bg = 'white')

inter_gg = dataf |> 
    mutate(cyn_binned = cut(viss_cynicism, 
                            breaks = 4)) |> 
    ggplot(aes(viss_objectivity, coss, 
               color = cyn_binned, 
               fill = cyn_binned,
               group = cyn_binned)) +
    geom_point(position = 'jitter', 
               alpha = .25) +
    stat_smooth(method = 'lm') +
    scale_color_viridis_d(end = .8, 
                          na.translate = FALSE, 
                          aesthetics = c('color', 'fill'))
inter_gg

## Putting all three into a single regression, effect of objectivity is not stat. sig. 
model_1 = lm(coss ~ viss_cynicism + 
                 viss_textbook + 
                 viss_objectivity, 
             data = dataf)
summary(model_1)
## Interaction of cynicism w/ objectivity doesn't change anything
model_2 = lm(coss ~ viss_cynicism + 
                 viss_textbook + 
                 viss_objectivity +
                 viss_cynicism * viss_objectivity, 
             data = dataf)
summary(model_2)

dataf$viss_objectivity |> 
    summary()

inter_reg_gg = predict_response(model_2, 
                 c('viss_cynicism', 'viss_objectivity'), 
                 margin = 'empirical') |> 
    plot() +
    scale_color_viridis_d(end = .8, 
                          aesthetics = c('color', 'fill'))
inter_reg_gg

inter_gg + inter_reg_gg

ggsave(here(out_dir, '05_interaction.png'), 
       height = 3, width = 6, scale = 1.5, bg = 'white')

inter_gt = list(model_1, model_2) |> 
    map(tbl_regression) |> 
    map(add_glance_table, 
        include = c('r.squared', 
                    'adj.r.squared', 
                    'AIC', 
                    'nobs')) |> 
    tbl_merge(tab_spanner = c('**Model 1**', '**Model 2**')) |> 
    modify_table_body(~.x %>% 
                          arrange(row_type == "glance_statistic"))

inter_gt |> 
    as_gt() |> 
    write_rds(here(out_dir, '05_inter.Rds'))

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

