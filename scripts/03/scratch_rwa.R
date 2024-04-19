## RWA ----
## alpha of combined RWA scale is 0.94
dataf |> 
    select(starts_with('rwa')) |> 
    select(!c(rwa.conservatism, 
              rwa.traditionalism, 
              rwa.authoritarianism, 
              rwa)) |> 
    psych::alpha(check.keys = TRUE)

## .88 for conservatism scale
dataf |> 
    select(starts_with('rwa.conservatism.')) |> 
    psych::alpha()
## .88 for traditionalism
dataf |> 
    select(starts_with('rwa.traditionalism.')) |> 
    psych::alpha()
## .85 for authoritarianism
dataf |> 
    select(starts_with('rwa.authoritarianism.')) |> 
    psych::alpha()

dataf |> 
    select(starts_with('rwa')) |> 
    visdat::vis_cor()

ggplot(dataf) +
    geom_autopoint(alpha = .1) +
    stat_smooth(aes(.panel_x, .panel_y), 
                method = 'lm') +
    # geom_autohistogram() +
    facet_matrix(vars(religious, politics, 
                      rwa.conservatism, 
                      rwa.traditionalism, 
                      rwa.authoritarianism, 
                      rwa), 
                 layer.diag = 3, 
                 grid.y.diag = FALSE)
    
