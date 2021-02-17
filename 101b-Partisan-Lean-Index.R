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
PREVIOUS_CYCLE <- 2020

# Graph Colors
PARTY_COL <- c('darkblue', 'red', 'green4')
PARTY_LEV <- c('dem', 'rep', 'other')



# Load Data --------------------------------------------------------------------
pres_results_prior <- read.csv('data/potus_results_46_20_dem_rep_other.csv') %>%
  select(-X) %>%
  mutate(party = factor(party, levels = PARTY_LEV))


# Calculate Average ------------------------------------------------------------
# National Results
pres_results_national <- pres_results_prior %>%
  filter(state == 'National') %>%
  select(cycle, party, nat_vote = vote_share)

# State Results
pres_results_states <- pres_results_prior %>%
  filter(!state == 'National') %>%
  select(cycle, state, state_abb, party, vote_share)

# Cycles for K Fold (first two cycles omitted since two cycle average)
CYCLES <- unique(pres_results_states$cycle)[-1:-2]

# Tune Parameter
TUNE <- seq(.5, 1, .001)

# Tune Function on prior cycles (ONLY RUN WHEN THERE IS NEW DATA, TAKES A LONG TIME)
# tune_output_k_fold <- map_df(CYCLES, function(C) {
#  tune_output <- map_df(TUNE, function (TN) {
#    rms <- pres_results_states %>%
#      left_join(pres_results_national) %>%
      # Filter Last Cycle
#      filter(cycle != C) %>%
#      mutate(national_deviation_index = vote_share - nat_vote) %>%
#      group_by(state, party) %>%
#      # Lag a Cycle behind for Error Check
#      mutate(expected_NDI = (lag(national_deviation_index) * TN) + (lag(national_deviation_index, 2) * (1 - TN)),
#             error = national_deviation_index - expected_NDI) %>%
#      na.omit() %>%
#      ungroup() %>%
#      dplyr::summarize(rms = mean(error^2)) %>%
#      .$rms
    
#    data.frame(tune = TN,
#               rms = rms)
#    })
  
#  weight <- tune_output$tune[tune_output$rms == min(tune_output$rms)]
  
#  rms <- pres_results_states %>%
#    left_join(pres_results_national) %>%
#    mutate(national_deviation_index = vote_share - nat_vote) %>%
#    group_by(state, party) %>%
#    mutate(expected_NDI = (national_deviation_index * weight) + (lag(national_deviation_index) * (1 - weight)),
#           error = national_deviation_index - expected_NDI) %>%
#    na.omit() %>%
#    ungroup() %>%
#    dplyr::summarize(rms = mean(error^2)) %>%
#    .$rms
  
#  data.frame(weight = weight,
#             rms = rms)
#})

# Best Error
# WEIGHT <- tune_output_k_fold$weight[tune_output_k_fold$rms == min(tune_output_k_fold$rms)]
# RMS <- tune_output_k_fold$rms[tune_output_k_fold$rms == min(tune_output_k_fold$rms)]

# save(WEIGHT, file = 'rda/WEIGHT.rda')
# save(RMS, file = 'rda/RMS.rda')

load('rda/WEIGHT.rda')
load('rda/RMS.rda')

sqrt(RMS)

# Use results to Calculate Expected Expected National Deviation Index
pres_results_states %>%
  left_join(pres_results_national) %>%
  mutate(national_deviation_index = vote_share - nat_vote) %>%
  group_by(state, party) %>%
  mutate(expected_NDI = (national_deviation_index * WEIGHT) + (lag(national_deviation_index) * (1 - WEIGHT)),
         expected_NDI_display = paste(party, ifelse(expected_NDI >= 0, ' +', ' -'), abs(round(expected_NDI * 100)), sep = '')) %>%
  arrange(desc(cycle))

NDIs_party_expected <- pres_results_states %>%
  left_join(pres_results_national) %>%
  mutate(national_deviation_index = vote_share - nat_vote) %>%
  group_by(state, party) %>%
  mutate(expected_NDI = (national_deviation_index * WEIGHT) + (lag(national_deviation_index) * (1 - WEIGHT)),
         expected_NDI_display = paste(party, ifelse(expected_NDI >= 0, ' +', ' -'), abs(round(expected_NDI * 100)), sep = '')) %>%
  arrange(desc(cycle)) %>%
  filter(cycle == PREVIOUS_CYCLE) %>%
  mutate(cycle = cycle + 4) %>%
  select(cycle, state, state_abb, party, expected_NDI) %>%
  ungroup

save(NDIs_party_expected, file = 'rda/NDIs_party_expected.rda')


# NDI Spreads All --------------------------------------------------------------
NDIs_spread_expected <- pres_results_states %>%
  left_join(pres_results_national) %>%
  group_by(cycle, state) %>%
  arrange(cycle, state) %>%
  mutate(state_spread = vote_share[2] - vote_share[1],
         nat_spread = nat_vote[2] - nat_vote[1],
         national_deviation_index = state_spread - nat_spread) %>%
  slice_head(n = 1) %>%
  select(-party:-nat_vote) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(expected_NDI = (lag(national_deviation_index) * WEIGHT) + (lag(national_deviation_index, 2) * (1 - WEIGHT)),
         NDI_error = national_deviation_index - expected_NDI) %>%
  na.omit() %>%
  arrange(desc(cycle)) %>%
  filter(cycle == PREVIOUS_CYCLE) %>%
  mutate(cycle = cycle + 4) %>%
  select(cycle, state, state_abb, expected_NDI) %>%
  ungroup

save(NDIs_spread_expected, file = 'rda/NDIs_spread_expected.rda')















