{# Libraries
  library(tidyverse)
  library(tidylog)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}
options(scipen = 999)

# Load Data --------------------------------------------------------------------
load('rda/modeled_EV_state_outcome_party.rda')
load('rda/NDIs_party_expected.rda')



# Average Models ---------------------------------------------------------------
# National Data (When There are no Polls to pull from)
national_start <- modeled_EV_state_outcome_party %>%
  filter(state == 'National') %>%
  select(party, nat_modeled_share = vote_share_mu)

# Expected NDI
expected_NDI_out <- NDIs_party_expected %>%
  left_join(national_start) %>%
  mutate(NDI_out = expected_NDI + nat_modeled_share) %>%
  select(state, state_abb, party, NDI_out)

# Form Ensemble
ensemble_rigid <- modeled_EV_state_outcome_party %>%
  filter(!state == 'National') %>%
  select(cycle, state, party, modeled_share = vote_share_mu) %>%
  left_join(expected_NDI_out) %>%
  select(cycle, state, state_abb, party, modeled_share, NDI_out) %>%
  mutate(ensemble_center = (modeled_share + NDI_out) / 2)

save(ensemble_rigid, file = 'rda/ensemble_rigid.rda')

# Table
ensemble_rigid %>%
  group_by(state) %>%
  mutate(ensemble_spread = ensemble_center[2] - ensemble_center[1]) %>%
  slice_head(n = 1) %>%
  select(cycle, state, state_abb, ensemble_spread) %>%
  mutate(ensemble_spread_display = paste(ifelse(ensemble_spread >= 0, 'R+', 'D+'), round(abs(ensemble_spread * 100)), sep = '')) %>%
  arrange(ensemble_spread) %>%
  kable()
