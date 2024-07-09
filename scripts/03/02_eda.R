library(tidyverse)
theme_set(theme_bw())
library(ggforce)
library(here)

dataf = read_rds(here('data', '03', '01_data.Rds'))

## 502 obs
nrow(dataf)

## Gender ----
count(dataf, gender) |> 
    mutate(share = n / sum(n)) |> 
    arrange(desc(n))

## Race-ethnicity ----
## 2021 census estimates: <https://en.wikipedia.org/wiki/Race_and_ethnicity_in_the_United_States#Racial_and_ethnic_categories>
## Overrep. NH White (70% vs. 60%), API (8% vs. 6%); maybe AIIAN (~2% vs. 0.7%)
## Underrep. Black (9% vs. 13%); Hispanic (6% vs. 19%)
count(dataf, race_ethnicity) |> 
    mutate(share = n / sum(n)) |> 
    arrange(desc(n))

## Politics ----
## Prolific's trinary variable, used for sampling
count(dataf, party) |> 
    mutate(share = n / sum(n))

## Our variable
## 1. "other" recoded as NA to simplify regressions
## 2. greater -> more conservative
count(dataf, politics)

ggplot(dataf, aes(politics)) +
    geom_bar()

ggplot(dataf, aes(politics, fill = party)) +
    geom_bar() +
    facet_wrap(vars(party))

## RWA
summary(dataf$rwa)

ggplot(dataf) +
    geom_density(aes(rwa)) +
    geom_density(aes(x = rwa.conservatism, 
                     color = 'conservatism')) +
    geom_density(aes(x = rwa.traditionalism, 
                     color = 'traditionalism')) +
    geom_density(aes(x = rwa.authoritarianism, 
                     color = 'authoritarianism'))

## TODO: plot grids
ggplot(dataf, aes(politics, rwa, 
                  group = politics)) +
    geom_violin(draw_quantiles = .5) +
    geom_sina()

## TODO: why is loess freaking out? 
## TODO: plot grids; religious only really corr w/ traditionalism
ggplot(dataf, aes(religious, rwa.authoritarianism, 
                  group = religious)) +
    geom_sina() +
    # geom_violin(color = 'red', fill = 'transparent', draw_quantiles = .5)
    stat_summary(color = 'red', size = 1)

## Religious attendance
## 54% none; might be high? 
## 19% "few times per year"
## 14% 2-3 times per month
count(dataf, religious) |> 
    mutate(share = n / sum(n))

