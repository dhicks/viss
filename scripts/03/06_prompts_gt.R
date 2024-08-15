library(tidyverse)
library(googlesheets4)
library(gt)
library(here)

sheet = read_sheet('https://docs.google.com/spreadsheets/d/1EuQbThPe7XCwU0SkZH9tZFy3_vTdN0vNHORpSTIbwrA/edit?gid=1444641995#gid=1444641995', 
                   sheet = 'comparison')

prompts_gt = sheet |> 
    select(!subscale) |> 
    mutate(modified = 
               if_else(`original text` != `finalized text` &
                           !is.na(`finalized text`), 
                       'X', 
                       '')) |> 
    gt() |> 
    sub_missing(missing_text = '') |> 
    cols_align(align = 'center', 
               columns = modified)
prompts_gt

write_rds(prompts_gt, here('out', '03', '06_prompts.Rds'))
gtsave(prompts_gt, here('out', '03', '06_prompts.html'))
gtsave(prompts_gt, here('out', '03', '06_prompts.pdf'))
gtsave(prompts_gt, here('out', '03', '06_prompts.tex'))

sheet |> 
    select(subscale, starts_with('final')) |> 
    filter(!is.na(`final item ID`)) |> 
    rename(`item ID` = `final item ID`, 
           `text` = `finalized text`) |> 
    arrange(subscale, `item ID`) |> 
    replace_na(list(subscale = 'not assigned to subscale')) |> 
    group_by(subscale) |> 
    gt() |> 
    
