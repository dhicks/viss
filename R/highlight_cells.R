highlight_cells = function(this_gt, 
                           col_select = where(is.numeric), 
                           cell_select = ~ abs(.) > 0.3, 
                           cell_style = cell_text(weight = 'bold')) {
    ## Identify columns
    cols = this_gt$`_data` |> 
        select({{ col_select }}) |> 
        colnames()
    
    ## Convert the cell selector formula into a function
    cell_selector <- rlang::as_function(cell_select)
    
    ## Iterate columnwise, applying the styling using the cell selector
    for (col in cols) {
        this_gt <- this_gt |> 
            tab_style(
                style = cell_style,
                locations = cells_body(
                    columns = all_of(col),
                    rows = cell_selector(.data[[col]]))
            )
    }
    return(this_gt)
}
