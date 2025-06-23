## todo
## - OSI scoring: DH fucked up and left out 6 questions

library(tidyverse)
library(qualtRics)
library(sjlabelled)

library(glue)
library(here)
library(openssl)
source(here('secrets', 'key.R'))

library(visdat)
library(skimr)

hash = function(pid) {
    sha224(pid, key = key) |> 
        as.character()
}

data_dir = here('data', '03')

## Retrieve Qualtrics responses ---
source(here(data_dir, 'survey_id.R'))

## Get column map
# fetch_survey(survey_id, limit = 1, add_column_map = TRUE) |>
#     extract_colmap() |>
#     view('colmap')

dataf_raw = fetch_survey(survey_id, 
                         label = TRUE, 
                         include_display_order = FALSE, 
                         breakout_sets = FALSE)

## For some reason, these OSI questions misbehave w/ 
## label = TRUE
osi_misbehave = fetch_survey(survey_id, 
                             include_questions = c('QID123', 
                                                   'QID124', 
                                                   'QID128'),
                             convert = FALSE) |> 
    select(PROLIFIC_PID, starts_with('osi'))

osi_answers = here(data_dir, 'osi_answers.csv') |> 
    read_csv()

## Occupation prestige scores
occ_prestige = here(data_dir, 'occ_prestige_scores.csv') |> 
    read_csv()

## Prolific data
prolific_df = here(data_dir, 'prolific_export_65fb2530deeba5f1ef0d945b.csv') |> 
    read_csv() |> 
    transmute(PROLIFIC_PID = `Participant id`, 
              party = `U.s. political affiliation`, 
              age = as_numeric(Age))

## Clean ----
dataf = dataf_raw |> 
    filter(Finished) |> 
    select(!c(`osi-lasers`, `osi-electrons`, 
              `osi-antibiotics`)) |> 
    left_join(osi_misbehave, by = 'PROLIFIC_PID') |> 
    left_join(prolific_df, by = 'PROLIFIC_PID') |> 
    transmute(
        prolific_id = PROLIFIC_PID,
        ## VISS
        across(aims.1:wait.policy, as_numeric), 
        ## Credibility of Science Scale
        across(starts_with('COSS', ignore.case = FALSE), 
               list(clean = ~ {.x |> 
                       factor(levels = c('Strongly disagree', 
                                         'Disagree', 
                                         'Somewhat disagree', 
                                         'Neither agree nor disagree', 
                                         'Somewhat agree', 
                                         'Agree', 
                                         'Strongly agree')) |> 
                       fct_rev() |> 
                       as_numeric(keep.labels = TRUE)})), 
        ## GSS-like "confidence in institutions"
        across(starts_with('GSS Confidence'), 
               ~ factor(.x, 
                        labels = c('A great deal', 
                                   'A lot', 
                                   'A moderate amount', 
                                   'A little', 
                                   'None at all')) |> 
                   fct_rev() |> 
                   as_numeric()), 
        ## Pew "effect of science on society"
        effect = {factor(`Pew effect`, 
                         levels = c('mostly negative', 
                                    'equally positive and negative', 
                                    'mostly positive')) |> 
                as_numeric()},
        ## Scientism
        across(starts_with('scientism'), 
               ~ {.x |> 
                       fct_rev() |> 
                       as_numeric()}),
        ## OSI 2.0
        across(starts_with('osi')),
        ## Demographics
        age, 
        gender_id = str_split(GenderIdentity, ','),
        gender_lived = str_split(GenderLived, ','), 
        gender = {map2(gender_id, gender_lived, union) |> 
                map_chr(~ str_c(.x, collapse = '; '))},
        race_ethnicity = {map(`Race/Ethnicity`, 
                              ~ {.x |> 
                                      str_split(',') |> 
                                      flatten_chr()}) |> 
                map(~ str_extract(.x, '^.*[:\\(]+'))  |> 
                map(~ keep(.x, ~ !is.na(.x))) |> 
                map(~ str_remove(.x, '[:\\(]+')) |> 
                map(~ str_c(.x, collapse = '; ')) |> 
                flatten_chr()}, 
        religious = as_numeric(ReligiousServ),
        occupation = `occupation-status _2`,
        ## Politics
        politics = {PoliticalIdeology |> 
                fct_recode(NULL = 'Other (please specify)') |> 
                as_numeric()},
        party,
        ## Reverse code parts of RWA
        across(c(`conservatism-1`, `conservatism-3`, 
                 `conservatism-6`, 
                 `traditionalism-1`, `traditionalism-4`, 
                 `traditionalism-6`, 
                 `authoritarianism-1`, `authoritarianism-3`, 
                 `authoritarianism-5`), 
               fct_rev),
        across(`conservatism-1`:`authoritarianism-6`,
               ~ {.x |> 
                       as_numeric()})
    ) |> 
    ## Renaming
    ## VISS
    rename(pluralism.2 = pluralism.3) |> 
    rename_with(~ glue('viss.{.x}'), 
                .cols = aims.1:wait.policy) |> 
    ## GSS confidence
    rename_with(.cols = starts_with('gss'), 
                .fn = ~ c('gss_college', 
                          'gss_congress', 
                          'gss_k12', 
                          'gss_companies', 
                          'gss_medicine', 
                          'gss_science')) |> 
    ## RWA
    rename_with(~ glue('rwa.{.x}'), 
                .cols = `conservatism-1`:`authoritarianism-6`) |> 
    ## Remove spaces, dashes
    rename_with(~ {.x |> 
            str_remove_all(' ') |> 
            str_replace_all('-', '.') |> 
            tolower()}) |> 
    rowwise() |> 
    ## Aggregations
    ## CoSS: mean: higher -> pro-science
    ## Scientism: mean: higher -> more scientistic
    ## RWA
    mutate(coss = mean(c_across(starts_with('coss'))), 
           scientism = mean(c_across(starts_with('scientism'))),
           rwa.conservatism = mean(c_across(starts_with('rwa.conservatism'))),
           rwa.traditionalism = mean(c_across(starts_with('rwa.traditionalism'))),
           rwa.authoritarianism = mean(c_across(starts_with('rwa.authoritarianism'))),
           rwa = mean(c_across(c(rwa.conservatism, 
                                 rwa.traditionalism, 
                                 rwa.authoritarianism)))
    ) |> 
    ungroup() |> 
    ## Occupation prestige
    left_join(occ_prestige, by = c('occupation' = 'occ_title')) |> 
    select(!c(occ_field, occ_prestige_field)) |> 
    rename(occ_prestige = occ_prestige_title) |> 
    ## Encrypt Prolific PID
    mutate(prolific_id = hash(prolific_id))

osi_scores = dataf |> 
    select(prolific_id, starts_with('osi')) |> 
    select(!c(osi.die, osi.bucks, osi.lillypad)) |>
    mutate(across(.fns = as_character)) |> 
    pivot_longer(-prolific_id, names_to = 'question', values_to = 'response') |> 
    inner_join(osi_answers, by = 'question') |> 
    mutate(correct = response == correct_resp) |> 
    # count(question, correct) |> filter(correct)
    group_by(prolific_id) |> 
    summarize(osi_score = sum(correct, na.rm = TRUE))

dataf = dataf |> 
    left_join(osi_scores, by = 'prolific_id')


## Missingness ----
## Relatively high rates of missingness for scientism; 
## A couple of participants dropped out and messaged about how they felt they couldn't answer these questions, eg, because they see religion and science as compatible
vis_miss(dataf) +
    coord_flip()

skim(dataf)

## Very high rate of occupation prestige missingness, nearly 1/3
dataf |> 
    count(is.na(occ_prestige)) |> 
    mutate(share = n /sum(n))

## Missing values are mostly for student, unemployed, retired, and homemaker titles
## Labor force participation rate was 63% in March 2024: 
## <https://www.bls.gov/charts/employment-situation/civilian-labor-force-participation-rate.htm>
## So this slightly undersamples people outside the workforce
dataf |> 
    select(occupation, occ_prestige) |> 
    filter(is.na(occ_prestige)) |> 
    count(occupation)

## Write out ----
write_rds(dataf, here(data_dir, '01_data.Rds'))
write_csv(dataf, here(data_dir, '01_data.csv'))
