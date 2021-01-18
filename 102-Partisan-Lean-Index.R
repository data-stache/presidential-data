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
pres_results_prior <- read.csv('data/potus_results_46_20_dem_rep_other.csv') %>%
  select(-X) %>%
  mutate(party = factor(party, levels = PARTY_LEV))


# Dem Rep Other ----------------------------------------------------------------
pres_results_national <- pres_results_prior %>%
  filter(state == 'National') %>%
  select(cycle, party, nat_vote = vote_share)

pres_results_states <- pres_results_prior %>%
  filter(!state == 'National') %>%
  select(cycle, state, state_abb, party, vote_share)

head(pres_results_national)
head(pres_results_states)

tune <- seq(.5, 1, .001)

# Tune on prior cycles
tune_output <- map_df(tune, function (X) {
  rms <- pres_results_states %>%
    left_join(pres_results_national) %>%
    # Filter Last Cycle
    filter(cycle != 2020) %>%
    mutate(national_deviation_index = vote_share - nat_vote) %>%
    group_by(state, party) %>%
    # Lag a Cycle behind for Error Check
    mutate(expected_NDI = (lag(national_deviation_index) * X) + (lag(national_deviation_index, 2) * (1-X)),
           error = index - expected_NDI) %>%
    na.omit() %>%
    ungroup() %>%
    summarize(rms = mean(error^2)) %>%
    .$rms
  
  data.frame(tune = X,
             rms = rms)
})

# Best Error
weight <- tune_output$tune[tune_output$rms == min(tune_output$rms)]
rms <- tune_output$rms[tune_output$rms == min(tune_output$rms)]

pres_results_states %>%
  left_join(pres_results_national) %>%
  mutate(national_deviation_index = vote_share - nat_vote) %>%
  group_by(state, party) %>%
  mutate(expected_NDI = (national_deviation_index * weight) + (lag(national_deviation_index) * (1 - weight)),
         expected_NDI_display = paste(party, ifelse(expected_NDI >= 0, ' +', ' -'), abs(round(expected_NDI * 100)), sep = '')) %>%
  arrange(desc(cycle))

expected_NDI <- pres_results_states %>%
  left_join(pres_results_national) %>%
  mutate(national_deviation_index = vote_share - nat_vote) %>%
  group_by(state, party) %>%
  mutate(expected_NDI = (national_deviation_index * weight) + (lag(national_deviation_index) * (1 - weight)),
         expected_NDI_display = paste(party, ifelse(expected_NDI >= 0, ' +', ' -'), abs(round(expected_NDI * 100)), sep = '')) %>%
  arrange(desc(cycle)) %>%
  filter(cycle == 2020) %>%
  mutate(cycle = cycle + 4) %>%
  select(cycle, state, state_abb, party, expected_NDI)

save(expected_NDI, file = 'rda/expected_NDI.rda')
















