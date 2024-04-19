## Descriptive analysis of VISS items
library(tidyverse)
library(visdat)
library(gghighlight)
library(plotly)
library(gt)

library(here)

source(here('R', 'vis_labels.R'))

data_dir = here("data")

## Functions ----
## Diverging bar plot
orient = function(count_df, level_col = response, ref_level = 3, value_col = n) {
    ## Set the orientation of each level's bar as `plot_value`
    ## The reference level gets two bars, one positive and one negative
    ## Based on <https://stackoverflow.com/questions/51201852/faceted-horizontal-divergent-stacked-bar-plot-including-negative-values-using-dp/51217969#51217969>
    ref_negative = count_df |> 
        filter({{level_col}} == ref_level) |> 
        mutate(plot_value = -{{value_col}}/2)
    
    count_df |> 
        mutate({{value_col}} := as_numeric({{value_col}}),
               plot_value     = case_when({{level_col}}  <  ref_level ~ -{{value_col}}, 
                                          {{level_col}} == ref_level  ~ {{value_col}}/2, 
                                          {{level_col}}  >  ref_level ~ {{value_col}})) |> 
        bind_rows(ref_negative)
}

## Sum of values of `vec` satisfying `condition`, eg, vec > 4
sum_if = function(vec, condition) {
    sum(vec[condition])
}


## Load data ----
d_clean = read_rds(here(data_dir, 'data.Rds')) |> 
    select(pid, ViS01:ViS36) |> 
    mutate(across(-pid, as.numeric)) |> 
    set_names('pid', vis_labels$tag)


## Quality checks ----
## 0.3% missing values
vis_miss(d_clean)
## Items with missing values don't appear to cluster
vis_miss(d_clean, cluster = TRUE)

## 906 complete cases; 
## 69 missing 1, 11 missing 2, 2 missing 3
d_clean |> 
    rowwise() |> 
    summarize(missing = sum(is.na(c_across(-pid)))) |> 
    count(missing)

dataf = na.omit(d_clean)

dataf_counts = dataf |> 
    pivot_longer(-pid, 
                 names_to = 'item', 
                 values_to = 'value') |> 
    count(item, value) |> 
    group_by(item) |> 
    mutate(share = n / sum(n)) |> 
    ungroup() |> 
    left_join(vis_labels, by = c('item' = 'tag'))

## Descriptive visualizations ----
## Share of respondents (agreeing or strongly agreeing)
ggplot(dataf_counts, aes(fct_rev(item), share, fill = as.factor(value))) +
    geom_col() +
    geom_hline(yintercept = .5, linetype = 'dashed') +
    gghighlight(value >= 4,
                unhighlighted_params = list(fill = NULL,
                                            alpha = .5)) +
    xlab('ViSS item') +
    scale_y_continuous(labels = scales::percent_format()) +
    # scale_fill_viridis_d(option = 'A', guide = 'none', direction = -1) +
    scale_fill_brewer(palette = 'RdBu', guide = 'none') +
    coord_flip() +
    theme_minimal()

## Diverging bar plot
dataf_counts |> 
    orient(level_col = value, value_col = share) |> 
    group_by(item) |> 
    mutate(agree_share = sum_if(share, value >= 4)) |> 
    ungroup() |> 
    arrange(agree_share, item, value) |> 
    mutate(item = fct_inorder(item), 
           share = scales::percent(share, accuracy = 1)) |> 
    ggplot(aes(item, 
               plot_value, 
               fill = as.factor(value), 
               label = share,
               text = prompt)) +
    geom_col(data = ~ filter(.x, plot_value > 0), 
             position = position_stack(reverse = TRUE)) +
    geom_col(data = ~ filter(.x, plot_value < 0), 
             position = position_stack()) +
    xlab('ViSS item') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'share of respondents') +
    scale_fill_brewer(palette = 'RdBu', guide = 'none') +
    coord_flip() +
    theme_minimal() #+
# theme(legend.position = 'none')

div_barplot_plotly = ggplotly(tooltip = c('x', 'label', 'text')) |> 
    hide_guides()
div_barplot_plotly

write_rds(div_barplot_plotly, here('out', '02_div_barplot.Rds'))

## Top/bottom 5 by agreement ----
agreement_df = dataf_counts |> 
    group_by(item, prompt) |> 
    summarize(agree_share = sum_if(share, value >= 4)) |> 
    ungroup() |> 
    arrange(desc(agree_share))

topbottom = agreement_df |> 
    slice(1:5, 32:36) |> 
    mutate(group = if_else(agree_share > .5, 'top 5', 'bottom 5')) |> 
    select(group, agree_share, item, prompt)

write_rds(topbottom, here('out', '02_topbottom.Rds'))

topbottom_gt = topbottom |> 
    group_by(group) |> 
    gt() |> 
    cols_label(agree_share = md('**(strongly)\nagree**'), 
               item = md('**item**'), 
               prompt = md('**prompt**')) |> 
    fmt_percent(agree_share, decimals = 0) |> 
    tab_style(cell_fill(color = 'blue', alpha = .25), 
              cells_body(rows = group == 'top 5')) |> 
    tab_style(cell_fill(color = 'red', alpha = .25), 
              cells_body(rows = group == 'bottom 5'))
topbottom_gt

gtsave(topbottom_gt, here('out', '02_topbottom.tex'))
gtsave(topbottom_gt, here('out', '02_topbottom.pdf'))


## VISS and demographics ----
#' Continuous and ordinal demographics
#+ fig.width = 10, fig.height = 8
dataf |> 
    ggplot(aes(x = .panel_x, y = .panel_y)) +
    ggpubr::stat_cor(label.x.npc = 'center', label.y.npc = 'center', 
                     geom = 'rect', alpha = .8,
                     mapping = aes(fill = after_stat(r), 
                                   x = .panel_x, y = .panel_y,
                                   xmin = -Inf, xmax = Inf, 
                                   ymin = -Inf, ymax = Inf)) +
    geom_jitter(alpha = .1) +
    geom_smooth(method = 'lm') +
    ggpubr::stat_cor(label.x.npc = 'right', label.y.npc = 'bottom', 
                     geom = 'label', hjust = 'right', vjust = 'bottom',
                     size = 5, 
                     r.accuracy = .01,
                     mapping = aes(label = after_stat(r.label), 
                                   fill = after_stat(r))) +
    ggforce::facet_matrix(rows = vars(starts_with('fa_')),
                          cols = vars(age, religious_serv, political_ideology, 
                                      political_affiliation, education), 
                          switch = 'y') +
    scale_fill_gradient2(limits = c(-1, 1)) +
    theme_bw()

#' 
dataf |> 
    # filter(!disclosure) |> 
    ggplot(aes(political_ideology, fa_cynicism)) +
    geom_beeswarm(aes(fill = as.factor(political_ideology)), 
                  size = 1L,
                  shape = 21L) +
    stat_summary(geom = 'line', color = 'black', size = 2) +
    scale_fill_brewer(type = 'div', palette = 'RdBu', 
                      direction = -1L, 
                      guide = 'none', 
                      aesthetics = c('color', 'fill')) +
    labs(x = ' ← more liberal     more conservative →\npolitical views',
         y = 'cynicism')

dataf |> 
    # filter(!disclosure) |> 
    ggplot(aes(political_ideology, fa_cynicism)) +
    geom_point(aes(fill = as.factor(political_ideology)), 
               position = 'jitter',
               shape = 21L, 
               alpha = .5) +
    stat_summary(geom = 'line', color = 'black', size = 2) +
    scale_fill_brewer(type = 'div', palette = 'RdBu', 
                      direction = -1L, 
                      guide = 'none', 
                      aesthetics = c('color', 'fill')) +
    labs(x = ' ← more liberal     more conservative →\npolitical views',
         y = 'cynicism')

ggsave(here(out_dir, '04_cynicism_polid.png'), 
       height = 3, width = 4, scale = 1, 
       bg = 'white')

dataf |> 
    # filter(!disclosure) |> 
    ggplot(aes(political_ideology, fa_scientism)) +
    geom_point(aes(fill = as.factor(political_ideology)), 
               position = 'jitter',
               shape = 21L, 
               alpha = .5) +
    stat_summary(geom = 'line', color = 'black', size = 2) +
    scale_fill_brewer(type = 'div', palette = 'RdBu', 
                      direction = -1L, 
                      guide = 'none', 
                      aesthetics = c('color', 'fill')) +
    labs(x = ' ← more liberal     more conservative →\npolitical views',
         y = 'scientism')

ggsave(here(out_dir, '04_scientism_polid.png'), 
       height = 3, width = 4, scale = 1, 
       bg = 'white')



#' Categorical demographics
#+ fig.width = 12, fig.height = 8
dataf |> 
    ggplot(aes(x = .panel_x, y = .panel_y)) +
    geom_boxplot(aes(group = .panel_y), varwidth = TRUE, 
                 orientation = NA) +
    ggforce::facet_matrix(cols = vars(starts_with('fa_')),
                          rows = vars(race_ethnicity, gender,
                                      religious_affil,
                                      part_values), 
                          switch = 'y') +
    theme_bw()
