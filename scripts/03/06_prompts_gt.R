library(tidyverse)
library(googlesheets4)
library(gt)
library(here)

prompts_gt = read_sheet('https://docs.google.com/spreadsheets/d/1EuQbThPe7XCwU0SkZH9tZFy3_vTdN0vNHORpSTIbwrA/edit?gid=1444641995#gid=1444641995', 
           sheet = 'comparison') |> 
    mutate(modified = 
               if_else(`original text` != `finalized text` &
                           !is.na(`finalized text`), 
                       'X', 
                       '')) |> 
    gt() |> 
    sub_missing(missing_text = '') |> 
    cols_align(align = 'center', 
               columns = modified)

write_rds(prompts_gt, here('out', '03', '06_prompts.Rds'))
gtsave(prompts_gt, here('out', '03', '06_prompts.html'))
gtsave(prompts_gt, here('out', '03', '06_prompts.pdf'))
gtsave(prompts_gt, here('out', '03', '06_prompts.tex'))
