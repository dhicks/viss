library(tidyverse)
theme_set(theme_bw())
library(sjlabelled)
library(ggforce)

library(here)

data_dir = here('data', '03')
dataf = read_rds(here(data_dir, '01_data.Rds')) |> 
    rowwise() |> 
    mutate(coss_var = mean(c_across(c(coss.1, 
                                      coss.2, 
                                      coss.3, 
                                      coss.6)))) |> 
    ungroup()

trust_vars = expr(c(coss, coss_var, gss_science, effect, scientism))

## Trust measures EDA ----
## CoSS ----
## CoSS 4 and 5 are negatively correlated w/ other items?????
dataf |> 
    select(starts_with('coss.')) |> 
    cor(use = 'pairwise.complete.obs')
ggplot(dataf) +
    geom_point(aes(x = .panel_x, y = .panel_y), 
               position = 'jitter', alpha = .1) +
    stat_smooth(aes(.panel_x, .panel_y), 
                method = 'lm') +
    facet_matrix(vars(starts_with('coss')))
## Reversing these, alpha is in the mid-.70s
dataf |> 
    select(starts_with('coss.')) |> 
    psych::alpha(check.keys = TRUE)

ggplot(dataf, aes(coss)) +
    geom_density()
ggplot(dataf, aes(coss_var)) +
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
## Generally moderate; 
## CoSS and the variant are highly correlated
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
demo_cont = expr(c(religious, politics, 
                   rwa.conservatism, rwa.traditionalism, 
                   rwa.authoritarianism, rwa, 
                   occ_prestige, osi_score))

## TODO: construct combined gender var
demo_cat = expr(c(gender_id, gender_lived, race_ethnicity, 
                  party))

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

