## Descriptive analysis of VISS items

#' ## VISS and demographics ##
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
