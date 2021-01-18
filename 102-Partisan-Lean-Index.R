{# Libraries
  library(lubridate)
  library(broom)
  library(knitr)
  library(Hmisc)
  library(tidyverse)
  library(tidylog)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}
options(scipen = 999)


# Master Variables -------------------------------------------------------------
RUN_DATE <- Sys.Date()
CURRENT_CYCLE <- 2024

# Graph Colors
PARTY_COL <- c('darkblue', 'red', 'green4')
PARTY_LEV <- c('dem', 'rep', 'other')



# Load Data --------------------------------------------------------------------
pres_results_prior <- read.csv('data/potus_results_76_20_tidy.csv') %>%
  select(-X)

pres_results_national <- pres_results_prior %>%
  filter(state == 'National' & party %in% c('dem', 'rep')) %>%
  select(cycle, party, nat_vote = vote_share)

pres_results_states <- pres_results_prior %>%
  filter(!state == 'National' & party %in% c('dem', 'rep')) %>%
  select(cycle, state, state_abb, party, vote_share)

head(pres_results_national)
head(pres_results_states)

tune <- seq(.5, 1, .01)

tune_output <- map_df(tune, function (X) {
  rms <- pres_results_states %>%
    left_join(pres_results_national) %>%
    filter(cycle != 2020) %>%
    mutate(index = vote_share - nat_vote) %>%
    group_by(state, party) %>%
    mutate(pred = (lag(index) * X) + (lag(index, 2) * (1-X)),
           error = index - pred) %>%
    na.omit() %>%
    ungroup() %>%
    summarize(rms = mean(error^2)) %>%
    .$rms
  
  data.frame(tune = X,
             rms = rms)
})

weight <- tune_output$tune[tune_output$rms == min(tune_output$rms)]
rms <- tune_output$rms[tune_output$rms == min(tune_output$rms)]

pres_results_states %>%
  left_join(pres_results_national) %>%
  mutate(index = vote_share - nat_vote) %>%
  group_by(state, party) %>%
  mutate(pred = (lag(index) * weight) + (lag(index, 2) * (1 - weight)),
         error = index - pred) %>%
  filter(party != 'other') %>%
  arrange(desc(cycle)) %>%
  filter(cycle == 2020) %>%
  kable












