library(car)
library(tidyverse)
theme_set(theme_minimal())
library(psych)
library(pander)
library(gvlma)
library(tables)
library(gghighlight)
library(lavaan)
library(ggpubr)
library(ggforce)
library(gt)

library(visdat)
library(grid)
library(plotly)

library(here)

data_dir = here('data', '01')
out_dir = here('out', '01')

source(here('R', 'vis_labels.R'))
source(here('R', 'loading_table.R'))
source(here('R', 'scores.R'))
source(here('R', 'highlight_cells.R'))

## Should the script overwrite existing loadings tables? 
overwrite_loading_tables = FALSE

## Load data ----
d_clean = read_rds(here(data_dir, 'data.Rds')) |> 
    select(pid, ViS01:ViS36) |> 
    mutate(across(-pid, as.numeric)) |> 
    set_names('pid', vis_labels$tag)

#EFA for the ViS items, checking factor structure; need to check if N is large enough for a split to do CFA on
d_vis <- d_clean |> 
    column_to_rownames('pid') |> 
    na.omit()

#if N is big enough to reasonably split for EFA and CFA,
set.seed(032585) #for reproducibility
dummy_sep <- rbinom(nrow(d_vis), 1, 0.5) #create dummy indicator to randomly split sample
d_vis_efa <- d_vis[dummy_sep == 0, ] #extract data where dummy == 0
d_vis_cfa <- d_vis[dummy_sep == 1, ] #extract data where dummy == 1

vis_cor(d_vis_efa)


## checking EFA assumptions ----
cor.matrix <- cor(d_vis_efa)
bartlett <- bartlett.test(d_vis_efa) #testing whether correlation matrix is significantly different from identity matrix
kmo <- KMO(d_vis_efa) #checking adequacy of sample size
det <- det(cor.matrix) #checking for possible multicollinearity



## EFA ----
vis_fa <- fa.parallel(d_vis_efa, fm = "minres", fa = "fa")

#check item loadings for N factor solutions (look for substantial cross-loadings or no substantial loadings)
#do for as many factor solution options as seems prudent; what is recommended by fa.parallel, visual inspection of scree, or eigenvalues
three_factor <- fa(d_vis_efa, nfactors = '3', rotate ="varimax")
six_factor <- fa(d_vis_efa, nfactors = '6', rotate ="varimax")

#writing factor loadings to a csv for easier inspection
three_clean = loading_table(three_factor, 
                            path = here(data_dir, 
                                        "three_factor_loadings.csv"), 
                            overwrite = overwrite_loading_tables)
six_clean = loading_table(six_factor, 
                          path = here(data_dir, 
                                      "six_factor_loadings.csv"), 
                          overwrite = overwrite_loading_tables)
six_clean

communalities <- 1 - apply(six_factor$loadings^2,1,sum)
communalities

## gt loadings table for paper ----
loadings_gt = six_factor |> 
    ## Extract loadings and tidy
    loadings() |> 
    unclass() |> 
    as_tibble(rownames = 'variable') |> 
    rename('MR1\ncynicism' = MR1, 
           'MR5\nscientism' = MR5, 
           'MR4\nVFI' = MR4,
           'MR2\ntextbook' = MR2,
           'MR6\nVIS' = MR6,
           'MR3\npower' = MR3) |> 
    ## Communalities
    mutate(communality = communalities) |> 
    gt() |> 
    fmt_number(columns = !variable,
               decimals = 2) |> 
    ## Cell highlighting helper; 
    ## uses a default threshold of ±0.3
    highlight_cells(col_select = starts_with('MR')) |> 
    highlight_cells(col_select = starts_with('MR'), 
                    cell_style = cell_fill('#F0F0F0'))
loadings_gt

write_rds(loadings_gt, here(out_dir, '03_loadings_gt.Rds'))
gtsave(loadings_gt, here(out_dir, '03_loadings_gt.html'))
gtsave(loadings_gt, here(out_dir, '03_loadings_gt.pdf'))
gtsave(loadings_gt, here(out_dir, '03_loadings_gt.tex'))

## CFA ----
#lavaan package for CFA
#specify the items in each latent variable/factor

#six factor model recommended by EFA parallel analysis and inflexion point on scree plot
six_factor_model <- 'scientism =~ scientism.1 + fallible.3 + ir.2 + aims.1 + technocracy.2 + factvalue.1
                    vis =~ ir.3 + aims.2 + aims.3
                    cynicism =~ coi.1 + consensus.3 + factvalue.2 + nonsubj.1 + fallible.2 + ir.1 + coi.2 + stdpt.1
                    power =~ stdpt.3 + coi.3 + stdpt.2
                    textbook =~ consensus.2 + fallible.1 + pluralism.3 + pluralism.1 + vfi.1
                    vfi =~ vfi.3 + nonsubj.2 + technocracy.1 + factvalue.3
                    '
fit6 <- cfa(six_factor_model, data = d_vis_cfa)
summary(fit6, fit.measures = TRUE)
fitmeasures(fit6, c('chisq','cfi','rmsea','rmsea.ci.upper','srmr','agfi'))

#+ fig.height = 8, fig.width = 8
score_grid(fit6, d_vis_cfa)

## Correlation matrix
fa_vars = c('scientism.1',
            'fallible.3',
            'ir.2',
            'aims.1',
            'technocracy.2',
            'factvalue.1',
            'ir.3',
            'aims.2',
            'aims.3',
            'coi.1',
            'consensus.3',
            'factvalue.2',
            'nonsubj.1',
            'fallible.2',
            'ir.1',
            'coi.2',
            'stdpt.1',
            'stdpt.3',
            'coi.3',
            'stdpt.2',
            'consensus.2',
            'fallible.1',
            'pluralism.3',
            'pluralism.1',
            'vfi.2',
            'vfi.3',
            'nonsubj.2',
            'technocracy.1',
            'factvalue.3')

text_scientism = textGrob('scientism', gp=gpar(fontsize=13, fontface="bold"))
text_vis = textGrob('VIS', gp=gpar(fontsize=13, fontface="bold"))
text_cynicism = textGrob('cynicism', gp=gpar(fontsize=13, fontface="bold"))
text_power = textGrob('power', gp=gpar(fontsize=13, fontface="bold"))
text_textbook = textGrob('textbook', gp=gpar(fontsize=13, fontface="bold"))
text_VFI = textGrob('VFI', gp=gpar(fontsize=13, fontface="bold"))

six_corr_plot = d_vis_cfa |> 
    cor() |> 
    as_tibble(rownames = 'item1') |> 
    pivot_longer(-item1, names_to = 'item2', values_to = 'cor') |> 
    filter_at(vars(item1, item2), ~ .x %in% fa_vars) |> 
    mutate(across(c(item1, item2), 
                  ~ fct_relevel(.x, fa_vars))) |> 
    ggplot(aes(item1, fct_rev(item2), fill = cor)) +
    geom_raster() +
    geom_hline(yintercept = cumsum(c(4, 5, 3, 8, 3, 6))+.5) +
    geom_vline(xintercept = cumsum(rev(c(4, 5, 3, 8, 3, 6)))+.5) +
    coord_equal(clip = 'off') +
    scale_fill_gradient2(limits = c(-1, 1), name = 'R') +
    labs(x = '', 
         y = '') +
    theme(#legend.position = c(.5, 0), legend.direction = 'horizontal',
        legend.position = 'right', legend.margin = margin(),
        axis.text.x = element_text(hjust = 1L, angle = 40, vjust = 1), 
        plot.margin = margin(l = 50, r = 10, t = 5, b = 0))
six_corr_plot

ann_x = -7
six_corr_out = six_corr_plot + 
    annotation_custom(text_scientism, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 27, ymax = 27) +
    annotation_custom(text_vis, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 22, ymax = 22) +
    annotation_custom(text_cynicism, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 17, ymax = 17) +
    annotation_custom(text_power, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 11, ymax = 11) +
    annotation_custom(text_textbook, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 7, ymax = 7) +
    annotation_custom(text_VFI, 
                      xmin = ann_x, xmax = ann_x, 
                      ymin = 2, ymax = 3)

six_corr_out

ggsave(here('out', '03_six_corr_matrix.png'), 
       plot = six_corr_out, bg = 'white',
       height = 5, width = 10, scale = 1)


#three factor model based on eigenvalues > 1
three_factor_model <- ' scientism =~ scientism.1 + scientism.3 + technocracy.2 + factvalue.3 + coi.3 + aims.1 + stdpt.2 + fallible.3
                        textbook =~ pluralism.1 + nonsubj.3 + vfi.2 + pluralism.3 + technocracy.1 + fallible.1 + consensus.2 + aims.3 + aims.2 + nonsubj.2
                        cynicism =~ coi.1 + consensus.3 + nonsubj.1 + ir.1 + stdpt.1 + coi.2
                        '
fit3 <- cfa(three_factor_model, data = d_vis_cfa)
summary(fit3, fit.measures = TRUE)
fitmeasures(fit3, c('chisq','cfi','rmsea','rmsea.ci.upper','srmr','agfi'))

score_grid(fit3, d_vis_cfa)


## Discrete version of 6-factor model ----
six_discrete = d_vis |> 
    rownames_to_column('pid') |> 
    rowwise() |>
    mutate(fa_scientism = mean(c_across(c(scientism.1, 
                                        fallible.3,
                                        ir.2,
                                        aims.1,
                                        technocracy.2,
                                        factvalue.1))),
           fa_vis = mean(c_across(c(ir.3,
                                  aims.2,
                                  aims.3))),
           fa_cynicism = mean(c_across(c(coi.1,
                                       consensus.3,
                                       factvalue.2,
                                       nonsubj.1,
                                       fallible.2,
                                       ir.1,
                                       coi.2, 
                                       stdpt.1))),
           fa_power = mean(c_across(c(stdpt.3,
                                    coi.3,
                                    stdpt.2))),
           fa_textbook = mean(c_across(c(consensus.2,
                                       fallible.1,
                                       pluralism.3,
                                       pluralism.1,
                                       vfi.1))),
           fa_vfi = mean(c_across(c(vfi.3,
                                  nonsubj.2,
                                  technocracy.1,
                                  factvalue.3)))) |> 
    ungroup()

score_grid(scores = six_discrete)
ggsave(here('out', 'six_score_grid.png'), 
       height = 5, width = 6, scale = 1.5)

write_csv(six_discrete, here(data_dir, 'fa_six.csv'))


ggplot(six_discrete, aes(fa_scientism)) +
    geom_density() +
    geom_boxplot(width = .05, position = position_nudge(y = -.05)) +
    scale_y_continuous(guide = 'none') +
    labs(x = 'scientism', 
         y = '')

ggsave(here('out', '03_scientism.png'), 
       height = 3, width = 4, scale = 1)

ggplot(six_discrete, aes(fa_cynicism)) +
    geom_density() +
    geom_boxplot(width = .05, position = position_nudge(y = -.05)) +
    scale_y_continuous(guide = 'none') +
    labs(x = 'cynicism', 
         y = '')

ggsave(here('out', '03_cynicism.png'), 
       height = 3, width = 4, scale = 1)
