library(tidyverse)
theme_set(theme_bw())
library(sjlabelled)
library(ggforce)
library(patchwork)
library(ggpubr)
library(gt)
library(ggExtra)
library(ggtext)

library(rlang)
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
                     name = '', 
                     labels = \(x) {label_wrap_gen()(x) %>% 
                         gsub('\\n', '<br>', x = .)}) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_brewer(palette = 'RdBu') +
    ## Extend axis ticks to left-dodged boxes
    ## NB modify yend to adjust length of tick
    ## <https://stackoverflow.com/questions/78722254/ggplot-extending-axis-ticks-and-putting-boxes-around-axis-items?noredirect=1#comment138794892_78722254>
    coord_flip(clip = 'off', ylim = c(0, NA), expand = FALSE) +
    annotate('segment', 
             y = 0, yend = -.125, 
             x = seq(2, 19, 2), 
             xend = seq(2, 19, 2), 
             linewidth = .5) +
    theme(legend.position = 'bottom', 
          plot.margin = margin(15, 10, 10, 10), 
          axis.text.y = element_markdown(
              box.color = 'black', 
              linetype = 1, 
              padding = unit(2, 'pt'), 
              linewidth = .25
          ))

ggsave(here(out_dir, '02_likert.png'), 
       height = 10, width = 16, bg = 'white')

## "Diverging" style
# counts_df |> 
#     filter(!is.na(response)) |> 
#     mutate(response = ordered(response)) |> 
#     # filter(item == 'viss.stdpt') |> 
#     orient(level_col = response, 
#            value_col = share, 
#            ref_level = 'Neither agree nor disagree') |> 
#     group_by(item) |> 
#     mutate(agree_share = sum_if(share, 
#                                 response >= "Somewhat agree")) |> 
#     ungroup() |> 
#     arrange(agree_share, item, response, plot_value) |> 
#     mutate(item = fct_inorder(prompt_wrap), 
#            share = scales::percent(share, accuracy = 1)) |> 
#     filter(response != 'Neither agree nor disagree') |> 
#     ggplot(aes(item, 
#                plot_value, 
#                fill = response, 
#                group = plot_order,
#                label = share,
#                text = prompt)) +
#     geom_col(position = position_stack(reverse = TRUE)) +
#     scale_x_discrete(name = 'ViSS item',
#                      guide = guide_axis(n.dodge = 2)) +
#     scale_y_continuous(labels = scales::label_percent(),
#                        breaks = scales::pretty_breaks(n = 8),
#                        name = 'share of respondents') +
#     scale_fill_brewer(palette = 'RdBu') +
#     coord_flip() +
#     theme(legend.position = 'bottom')
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

## COI ----
coi_plot = {ggplot(viss_df, aes(coi.1, coi.2)) +
        geom_point(position = 'jitter') +
        geom_smooth(method = 'lm') +
        stat_cor(aes(label = after_stat(r.label)), 
                 digits = 1, 
                 label.x = 6, label.y = 2,
                 geom = 'label')} |>   
    ggMarginal(type = 'histogram', binwidth = 1, 
               fill = 'transparent')
coi_plot

ggsave(here(out_dir, '02_coi.png'), coi_plot,
       height = 4, width = 4, bg = 'transparent')

## VFI and critiques ----
vfi = exprs(nonsubj.1, nonsubj.2,
            vfi.1, vfi.2)
crits = exprs(aims.2, 
              ir,
              stdpt, 
              value.conflict)


center_gg = ggplot(viss_df) +
    geom_autopoint(position = 'jitter', alpha = .2) +
    stat_smooth(aes(.panel_x, .panel_y), 
                method = 'lm') +
    stat_cor(aes(.panel_x, .panel_y,
                 label = after_stat(label)), 
             alternative = 'less',
             geom = 'label',
             label.y = 1, 
             digits = 1) +
    facet_matrix(rows = vars(!!!vfi), 
                 cols = vars(!!!crits))
center_gg

side_gg = viss_df |> 
    select(!!!vfi) |> 
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
    select(!!!crits) |> 
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
     BBBBC
     BBBBC
     BBBBC'
top_gg + center_gg + side_gg +
    plot_layout(design = design)

ggsave(here(out_dir, '02_vfi.png'), 
       height = 16, width = 16, bg = 'white')


## Table of CIs on correlations
cor_ci = function(col1, col2, data = viss_df) {
    col1 <- enquo(col1)
    col2 <- enquo(col2)
    
    col1_vec <- as.vector(eval_tidy(col1, data))
    col2_vec <- as.vector(eval_tidy(col2, data))
    
    cor.test(col1_vec, col2_vec) |> 
        broom::tidy() |> 
        mutate(col1 = as_label(col1), 
               col2 = as_label(col2)) |> 
        select(col1, col2, everything())
}
# cor_ci(vfi.1, aims.2)

cross2(vfi, crits) |> 
    map(~ cor_ci(!!.x[[1]], !!.x[[2]])) |> 
    bind_rows() |> 
    select(vfi = col1, critique = col2, estimate, conf.low, conf.high)

count(viss_df, vfi.1 > 4, ir > 4) |> 
    mutate(share = n / sum(n))
count(viss_df, nonsubj.1 > 4, stdpt > 4) |> 
    mutate(share = n / sum(n))


## VFI and trust ----
trust_vars = exprs(coss)
# vfi_trust_gg = dataf |> 
#     rename_with(~ str_remove(.x, 'viss.')) |> 
#     ggplot() +
#     geom_autopoint(position = 'jitter', alpha = .2) +
#     stat_smooth(aes(.panel_x, .panel_y), 
#                 method = 'lm') +
#     stat_cor(aes(.panel_x, .panel_y,
#                  label = after_stat(label)), 
#              geom = 'label',
#              label.y = 1, 
#              digits = 1) +
#     facet_matrix(rows = vars(!!!trust_vars), 
#                  cols = vars(!!!vfi, !!!crits))
# vfi_trust_gg

## Alternate version for COSS alone
coss_plot = function(col_vars) {
    dataf |> 
        rename_with(~ str_remove(., 'viss.')) |> 
        ggplot() +
        geom_autopoint(position = 'jitter', alpha = .2) +
        stat_smooth(aes(.panel_x, .panel_y), 
                    method = 'lm') +
        stat_cor(aes(.panel_x, .panel_y, 
                     label = after_stat(label)), 
                 geom = 'label', 
                 label.y = 1, 
                 digits = 1) +
        facet_matrix(rows = vars(coss), 
                     cols = vars(!!! col_vars))
}
coss_plot(vfi)
coss_plot(crits)

trust_hist_gg = dataf |> 
    select(!!!trust_vars) |> 
    pivot_longer(everything(), names_to = 'item', values_to = 'response') |> 
    ggplot(aes(response)) +
    geom_histogram(binwidth = .25) +
    coord_flip() +
    labs(x = '') +
    facet_grid(rows = vars(item), scales = 'free_y')
trust_hist_gg

# vfi_trust_gg + trust_hist_gg + plot_layout(widths = c(length(c(vfi, crits)), 1))
# ggsave(here(out_dir, '02_trust.png'), 
#        height = 2, width = length(c(vfi, crits)) + 1, bg = 'white', scale = 1.5)

coss_plot(vfi) + trust_hist_gg + 
    coss_plot(crits) + trust_hist_gg +
    plot_layout(design = 'AAAAB
                          CCCCD', 
                axes = 'collect')
ggsave(here(out_dir, '03_trust.png'), 
       height = 4, width = 9, bg = 'white', scale = 1.5)

summary(dataf$coss)
