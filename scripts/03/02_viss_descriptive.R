library(tidyverse)
theme_set(theme_bw())
library(sjlabelled)
library(ggforce)
library(patchwork)
library(ggpubr)
library(gt)

library(here)
library(glue)

data_dir = here('data', '03')
out_dir = here('out', '03')

dataf = read_rds(here(data_dir, '01_data.Rds'))
viss_df = select(dataf, prolific_id, starts_with('viss')) |> 
    rename_with(.cols = starts_with('viss'), 
                .fn = ~ str_remove(.x, 'viss.'))

wrap_prompt = function(item, prompt) {
    item |> 
        str_remove('viss.') %>% 
        {glue('{.}: {prompt}')} |> 
        str_wrap(width = 25) |> 
        fct_inorder()
}

prompts_df = viss_df |> 
    get_label() |> 
    enframe('item', 'prompt') |> 
    slice(-1) |> 
    mutate(prompt = fct_inorder(prompt), 
           prompt_wrap = wrap_prompt(item, prompt))

likert = get_labels(viss_df$aims.1)

source(here('R', 'div_bar.R'))


## Share of respondents ----
counts_df = viss_df |> 
    pivot_longer(-prolific_id, 
                 names_to = 'item', 
                 values_to = 'response') |> 
    count(item, response) |> 
    mutate(response = factor(response, labels = likert)) |> 
    group_by(item) |> 
    mutate(share = n / sum(n)) |> 
    ungroup() |> 
    left_join(prompts_df, by = 'item')

## "Fill" style
counts_df |> 
    group_by(item) |> 
    mutate(agree = {if_else(as.numeric(response) > 3, 
                            share, 
                            0) |> 
            sum(na.rm = TRUE)}) |> 
    ungroup() |> 
    mutate(prompt_wrap = fct_reorder(prompt_wrap, agree)) |> 
    ggplot(aes(prompt_wrap, share, fill = response)) +
    geom_col(color = 'black', size = .25) +
    geom_hline(yintercept = c(.2, .5, .8), linetype = 'dashed') +
    scale_x_discrete(guide = guide_axis(n.dodge = 2), 
                     name = '') +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_brewer(palette = 'RdBu') +
    coord_flip() +
    theme(legend.position = 'bottom', 
          plot.margin = margin(15, 10, 10, 10))

ggsave(here(out_dir, '02_likert.png'), 
       height = 9, width = 16, bg = 'white')

## "Diverging" style
counts_df |> 
    filter(!is.na(response)) |> 
    mutate(response = ordered(response)) |> 
    # filter(item == 'viss.stdpt') |> 
    orient(level_col = response, 
           value_col = share, 
           ref_level = 'Neither agree nor disagree') |> 
    group_by(item) |> 
    mutate(agree_share = sum_if(share, 
                                response >= "Somewhat agree")) |> 
    ungroup() |> 
    arrange(agree_share, item, response, plot_value) |> 
    mutate(item = fct_inorder(prompt_wrap), 
           share = scales::percent(share, accuracy = 1)) |> 
    filter(response != 'Neither agree nor disagree') |> 
    ggplot(aes(item, 
               plot_value, 
               fill = response, 
               group = plot_order,
               label = share,
               text = prompt)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_x_discrete(name = 'ViSS item',
                     guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous(labels = scales::label_percent(),
                       breaks = scales::pretty_breaks(n = 8),
                       name = 'share of respondents') +
    scale_fill_brewer(palette = 'RdBu') +
    coord_flip() +
    theme(legend.position = 'bottom')
# plotly::ggplotly()

## Top and bottom table ----
agreement_df = counts_df |> 
    group_by(item, prompt) |> 
    summarize(agree_share = sum_if(share, 
                                   as.numeric(response) >= 4, 
                                   na.rm = TRUE)) |> 
    ungroup() |> 
    arrange(desc(agree_share))

topbottom = agreement_df |> 
    slice(1:5, 15:19) |> 
    mutate(group = if_else(agree_share > .5, 'top 5', 'bottom 5')) |> 
    select(group, agree_share, item, prompt) |> 
    mutate(item = str_remove(item, 'viss.'))

write_rds(topbottom, here(out_dir, '02_topbottom.Rds'))

topbottom_gt = topbottom |> 
    group_by(group) |> 
    gt() |> 
    cols_label(agree_share = md('**agreement**'), 
               item = md('**item**'), 
               prompt = md('**prompt**')) |> 
    fmt_percent(agree_share, decimals = 0) |> 
    tab_style(cell_fill(color = 'blue', alpha = .25), 
              cells_body(rows = group == 'top 5')) |> 
    tab_style(cell_fill(color = 'red', alpha = .25), 
              cells_body(rows = group == 'bottom 5'))
topbottom_gt

gtsave(topbottom_gt, here(out_dir, '02_topbottom.tex'))
gtsave(topbottom_gt, here(out_dir, '02_topbottom.pdf'))


## VFI and critiques ----
vfi = expr(c(nonsubj.1, 
             vfi.1))
crits = expr(c(aims.2, 
               ir,
               stdpt, 
               value.conflict))

# corrr
# corr_df = viss_df |> 
#     select(!!vfi, !!crits) |> 
#     correlate() |> 
#     focus(!!crits)

## TODO: table
# corr_df |> 
#     gt()
    

center_gg = ggplot(viss_df) +
    geom_autopoint(position = 'jitter', alpha = .2) +
    stat_smooth(aes(.panel_x, .panel_y), 
                 method = 'lm') +
    stat_cor(aes(.panel_x, .panel_y,
                         label = after_stat(r.label)), 
                     geom = 'label',
                     label.y = 1, 
             digits = 1) +
    facet_matrix(rows = vars(!!vfi), 
                 cols = vars(!!crits))
center_gg

side_gg = viss_df |> 
    select(!!vfi) |> 
    pivot_longer(everything(), 
                 names_to = 'item', 
                 values_to = 'resp') |> 
    ggplot(aes(resp)) +
    geom_histogram() +
    facet_grid(rows = vars(item)) +
    coord_flip() +
    labs(x = '', y = '')
side_gg

top_gg = viss_df |> 
    select(!!crits) |> 
    pivot_longer(everything(), 
                 names_to = 'item', 
                 values_to = 'resp') |> 
    ggplot(aes(resp)) +
    geom_histogram() +
    facet_grid(cols = vars(item)) +
    labs(x = '', y = '')
top_gg

design = 
    'AAAA#
     BBBBC
     BBBBC'
top_gg + center_gg + side_gg +
    plot_layout(design = design)

ggsave(here(out_dir, '02_vfi.png'), 
       height = 9, width = 16, bg = 'white')
