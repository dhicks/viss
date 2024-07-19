library(tidyverse)
theme_set(theme_bw())
library(sjlabelled)
library(here)
library(glue)
library(patchwork)

homophones = c('aims.1', 'aims.3', 
               'coi.1', 'consensus.2', 
               'nonsubj.1')
## study3 = study1
renumber = c('consensus.1' = 'consensus.3',
             'pluralism.2' = 'pluralism.3',
             'vfi.1' = 'vfi.3', 
             'wait.policy' = 'technocracy.1')

## Study 1 data ----
source(here('R', 'vis_labels.R'))
dataf_1 = here('data', '01', 'data.Rds') |> 
    read_rds() |> 
    select(pid, ViS01:ViS36) |> 
    mutate(across(-pid, as.numeric)) |> 
    set_names('prolific_id', vis_labels$tag) |> 
    mutate(study = 1L) |> 
    rename_with(.cols = any_of(c(homophones, names(renumber))), 
                .fn = ~ str_c(.x, '-1')) |> 
    rename(all_of(renumber))

count(dataf_1, aims.2)

agree_1 = dataf_1 |>
    group_by(study) |> 
    summarize(across(!prolific_id, 
                     .fns = list(agree = ~ mean(.x > 3, na.rm = TRUE), 
                                 n = ~ sum(!is.na(.x))), 
                     .names = '{.col}&{.fn}')) |> 
    pivot_longer(cols = c(ends_with('&agree'), 
                          ends_with('&n')), 
                 names_to = c('item', 'stat'), 
                 names_sep = '&',
                 values_to = 'value') |> 
    pivot_wider(names_from = 'stat', values_from = 'value')


## Study 3 data ----
dataf_3 = here('data', '03', '01_data.Rds') |> 
    read_rds() |> 
    select(prolific_id, starts_with('viss')) |> 
    rename_with(.cols = starts_with('viss'), 
                .fn = ~ str_remove(.x, 'viss.')) |> 
    mutate(study = 3L)

count(dataf_3, aims.1)

agree_3 = dataf_3 |> 
    group_by(study) |> 
    summarize(across(!prolific_id, 
                     .fns = list(agree = ~ mean(.x > 4, na.rm = TRUE), 
                                 n = ~ sum(!is.na(.x))))) |> 
    pivot_longer(cols = c(ends_with('_agree'), 
                          ends_with('_n')), 
                 names_to = c('item', 'stat'), 
                 names_sep = '_',
                 values_to = 'value') |> 
    pivot_wider(names_from = 'stat', values_from = 'value')


wrap_prompt = function(item, prompt) {
    item |> 
        str_remove('viss.') %>% 
        {glue('{.}: {prompt}')} |> 
        str_wrap(width = 25) |> 
        fct_inorder()
}

prompts_3 = dataf_3 |> 
    get_label() |> 
    enframe('item', 'prompt') |> 
    slice(-1) |> 
    mutate(prompt = fct_inorder(prompt), 
           prompt_wrap = wrap_prompt(item, prompt))

## Combine ----
dataf = bind_rows(agree_1, agree_3) |> 
    mutate(moe = qnorm(.975) * sqrt(agree * (1-agree) / n), 
           ci.lo = agree - moe, 
           ci.hi = agree + moe, 
           study = as.character(study))

agree_gg = ggplot(dataf, aes(fct_rev(item), agree, 
                             color = study, 
                             shape = study)) +
    geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi), 
                    position = position_dodge(width = .5)) +
    labs(y = 'agreement', x = 'VISS item') +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_color_brewer(palette = 'Set1') +
    coord_flip()
agree_gg


used_gg = dataf |> 
    group_by(study, item) |> 
    summarize(used = n > 0) |> 
    ungroup() |> 
    # complete(study, item, fill = list(used = FALSE)) |> 
    ggplot(aes(study, fct_rev(item), 
               color = study)) +
    # geom_raster() +
    # scale_fill_manual(values = c('transparent', 'blue'), 
    # guide = 'none') +
    # geom_text(label = '\u2705', family = 'Apple Color Emoji', 
    #           position = position_nudge(y = -.5)) +
    geom_point(aes(shape = 'a'), size = 4) +
    scale_shape_manual(values = c('\u2713'),
                       guide = 'none') +
    scale_color_brewer(palette = 'Set1', guide = 'none') +
    theme(panel.grid = element_blank(), 
          panel.grid.major.y = element_line('grey92')) +
    labs(x = 'study') + 
    scale_y_discrete(name = 'VISS item', guide = 'none')
used_gg
# ggsave(here('out', '03', '07_test.png'), plot = used_gg)

agree_gg + used_gg +
    plot_layout(widths = c(5, 1), 
                axes = 'collect', 
                guides = 'collect') &
    theme(legend.position = 'bottom')

ggsave(here('out', '03', '07_desc_comb.png'), 
       height = 7, width = 7, scale = 1, bg = 'white')

# Total agreement on VISS items.  Right panel indicates whether the item appeared in study 1, study 3, or both.  In the main panel, point estimates and 95% confidence intervals for rates of total agreement ("somewhat agree" or more).  VISS items are primarily labeled per the finalized versions used in study 3.  Where labels would overlap but the text is different between studies (either due to rewording or relabeling), the version used in study 1 is labeled as '-1'.  

# @fig-desc-comb compares rates of agreement ("somewhat agree" or more) for all VISS items across studies 1 and 3.  Rates of agreement were generally not statistically significant; and even statistically significant differences were small. The largest difference was for aims.2, with 75% agreement in study 1 and 82% in study 3; followed by pluralism.1, with 19% vs. 26%.  All other differences were 5 percentage points or less in absolute value.  

dataf |> 
    filter(item == 'aims.2')

dataf |> 
    pivot_wider(id_cols = 'item', 
                names_from = 'study', 
                values_from = 'agree') |> 
    mutate(diff = abs(`3` - `1`)) |> 
    arrange(desc(diff))
