library(tidyverse)
theme_set(theme_bw())

library(here)

## HendriksMeasuringLaypeopleTrust2015

dataf = here('data', '01', 'data.Rds') |> 
    read_rds() |> 
    select(pid, disclosure, sci_values, part_values, 
           starts_with('meti', ignore.case = FALSE)) |> 
    mutate(sci_values = if_else(disclosure, 
                                sci_values, 
                                'not disclosed'))

## A scientist who discloses public health values might be seen as a little more benevolent, competent, and honest than a scientist who doesn't disclose
## A scientist who discloses counter-aim values might be seen as slightly less competent, but much less benevolent and honest
dataf |> 
    rename(meti_overall = meti_mean) |> 
    pivot_longer(starts_with('meti'), 
                 names_to = 'aspect', 
                 names_prefix = 'meti_', 
                 values_to = 'response') |> 
    ggplot(aes(x = sci_values, 
               y = response, 
               color = aspect, shape = aspect,
               group = aspect)) +
    stat_summary(position = position_dodge(width = .2),
                 fun.data = mean_cl_boot,
                 linewidth = 1, size = .8) +
    stat_summary(position = position_dodge(width = .2),
                 fun = mean,
                 geom = 'line',
                 show.legend = FALSE,
                 linewidth = 1) +
    # scale_color_brewer(palette = 'RdBu') +
    ggthemes::scale_colour_tableau() +
    # scale_color_viridis_d(option = 'C') +
    scale_alpha_continuous(guide = 'none') +
    labs(x = "scientist's values", 
         y = 'response\n(1-7)',
         color = 'METI dimension', 
         shape = 'METI dimension', 
         caption = 'Mean and 95% CI; N = 988')

ggsave(here('out', '01', '04_dims.png'), 
       height = 4, width = 8, bg = 'white')

# last_plot() + 
#     gghighlight::gghighlight(aspect == 'overall')

