library(tidyverse)
theme_set(theme_bw())
library(sjlabelled)

library(here)
library(glue)

data_dir = here('data', '03')

dataf = read_rds(here(data_dir, '01_data.Rds'))
viss_df = select(dataf, prolific_id, starts_with('viss'))

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

likert = get_labels(viss_df$viss.aims.1)

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
ggplot(counts_df, aes(fct_rev(prompt_wrap), share, fill = response)) +
    geom_col() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2), 
                     name = '') +
    scale_y_continuous(labels = scales::percent_format()) +
    # scale_fill_viridis_d(option = 'A', guide = 'none', direction = -1) +
    scale_fill_brewer(palette = 'RdBu') +
    coord_flip() +
    theme(legend.position = 'bottom')

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
