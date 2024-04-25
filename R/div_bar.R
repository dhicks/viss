## Diverging bar plot
orient = function(count_df, level_col = response, ref_level = 3, value_col = n) {
    ## Set the orientation of each level's bar as `plot_value`
    ## The reference level gets two bars, one positive and one negative
    ## Based on <https://stackoverflow.com/questions/51201852/faceted-horizontal-divergent-stacked-bar-plot-including-negative-values-using-dp/51217969#51217969>
    ref_negative = count_df |> 
        filter({{level_col}} == ref_level) |> 
        mutate(plot_value = -{{value_col}}/2)
    
    count_df |> 
        mutate({{value_col}} := as.numeric({{value_col}}),
               plot_value     = case_when({{level_col}}  <  ref_level ~ -{{value_col}}, 
                                          {{level_col}} == ref_level  ~ {{value_col}}/2, 
                                          {{level_col}}  >  ref_level ~ {{value_col}})) |> 
        bind_rows(ref_negative) |> 
        mutate(plot_order = as_numeric({{ level_col }}), 
               plot_order = case_when(plot_value < 0 ~ -plot_order, 
                                      plot_value > 0 ~ plot_order))
}

## Sum of values of `vec` satisfying `condition`, eg, vec > 4
sum_if = function(vec, condition, ...) {
    sum(vec[condition], ...)
}
