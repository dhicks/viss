library(tidyverse)
library(googlesheets4)
library(gt)
library(here)

sheet = read_sheet('https://docs.google.com/spreadsheets/d/1EuQbThPe7XCwU0SkZH9tZFy3_vTdN0vNHORpSTIbwrA/edit?gid=1444641995#gid=1444641995', 
                   sheet = 'comparison')

prompts_gt = sheet |> 
    select(subscale, 
           id = `final item ID`, 
           text = `finalized text`) |> 
    filter(!is.na(id)) |> 
    mutate(subscale = subscale |> 
               as_factor() |> 
               fct_na_value_to_level(
                   level = 'not assigned')) |> 
    group_by(subscale) |> 
    gt() |> 
    row_group_order(c('cynicism', 
                      'objectivity', 
                      'textbook')) |> 
    tab_style(cell_text(weight = 'bold'), 
              cells_row_groups())

prompts_gt

write_rds(prompts_gt, here('out', '03', '06_prompts.Rds'))
gtsave(prompts_gt, here('out', '03', '06_prompts.html'))
gtsave(prompts_gt, here('out', '03', '06_prompts.pdf'))
gtsave(prompts_gt, here('out', '03', '06_prompts.tex'))


compare_gt = sheet |> 
    select(!subscale) |> 
    mutate(modified = 
               if_else(`original text` != `finalized text` &
                           !is.na(`finalized text`), 
                       '✓', 
                       '')) |> 
    gt() |> 
    sub_missing(missing_text = '') |> 
    cols_align(align = 'center', 
               columns = modified)
compare_gt

write_rds(compare_gt, here('out', '03', '06_compare.Rds'))
gtsave(compare_gt, here('out', '03', '06_compare.html'))
gtsave(compare_gt, here('out', '03', '06_compare.pdf'))
gtsave(compare_gt, here('out', '03', '06_compare.tex'))
