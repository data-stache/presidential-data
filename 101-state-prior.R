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


# State Presidential Election Data ---------------------------------------------
# Load Election Results since 1976
pres_results_prior <- read.csv("data/potus_results_76_20_tidy.csv", stringsAsFactors = FALSE, header = TRUE) %>% 
  # Remove X Column - not sure why it shows up
  select(-X) %>%
  # Arrange by election year, then by state
  arrange(cycle, state) %>%
  # Make Party a factor for aesthetic mappings / easier maths
  mutate(party = factor(party, levels = PARTY_LEV),
         cycle_weight = (CURRENT_CYCLE - cycle) / 4,
         cycle_weight = .5 ^ ((cycle_weight - 1) / 3))

pres_results_prior <- pres_results_prior[, c(1, 12, 2:11)]

# Election Summary Spread Statistics
pres_results_prior %>%
  group_by(cycle, state) %>%
  mutate(spread = vote_share[2] - vote_share[1]) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(state) %>%
  select(cycle, state, spread) %>%
  summarize(mean = mean(spread, na.rm = TRUE),
            sd = sd(spread, na.rm = TRUE)) %>%
  mutate(mean_mean = mean(mean),
         mean_sd = mean(sd)) %>%
  arrange(sd) %>%
  kable()

save(pres_results_prior, file = 'rda/pres_results_prior.rda')



# Three Party State Level Linear Models ----------------------------------------
# Unique States
STATES <- unique(pres_results_prior$state)
PARTIES <- unique(pres_results_prior$party)

state_party_fit <- map_df(STATES, function(ST) {
  map_df(PARTIES, function(PY) {
    fit <- pres_results_prior %>%
      select(state, cycle, party, vote_share, cycle_weight) %>%
      filter(state == ST & party == PY) %>%
      lm(vote_share ~ cycle, weights = cycle_weight, data = .)
    
    data <- data.frame(state = paste(ST),
                       party = paste(PY),
                       cycle = 2024)
    
    pred <- predict.lm(fit, data, se.fit = TRUE)
    
    data.frame(state = paste(ST),
               cycle = 2024,
               party = paste(PY),
               pred_vote_share = pred$fit,
               pred_se = pred$se.fit,
               pred_df = pred$df,
               pred_res = pred$residual.scale)
})
})

# Clear Row Names
row.names(state_party_fit) <- c()

head(state_party_fit)

# Prior Summary Stats
modeled_EV_state_outcome_party <- state_party_fit %>%
  mutate(run_date = RUN_DATE,
         party = factor(party, levels = PARTY_LEV),
         vote_share_mu = pred_vote_share,
         # 90% Confidence Interval, T-Test Z Score
         vote_share_z = qt(0.95, pred_df),
         vote_share_t_dist = vote_share_z * pred_se,
         vote_share_start = vote_share_mu - vote_share_t_dist,
         vote_share_end = vote_share_mu + vote_share_t_dist,
         cycle = 2024) %>%
  select(run_date, cycle, state, party, vote_share_mu, vote_share_se = pred_se, vote_share_t_dist, vote_share_start, vote_share_end, vote_share_z)

head(modeled_EV_state_outcome_party)

save(modeled_EV_state_outcome_party, file = 'rda/modeled_EV_state_outcome_party.rda')

write.csv(modeled_EV_state_outcome_party, file = 'out/modeled_EV_state_outcome_party.csv')



# State Level Spread Linear Models ---------------------------------------------
# Unique States
STATES <- unique(pres_results_prior$state)

state_fit <- map_df(STATES, function(ST) {
  fit <- pres_results_prior %>%
    filter(state == ST) %>%
    group_by(cycle) %>%
    mutate(spread = vote_share[2] - vote_share[1]) %>%
    slice(1) %>%
    select(state, cycle, spread, cycle_weight) %>%
    lm(spread ~ cycle, weights = cycle_weight, data = .)
    
  data <- data.frame(state = paste(ST),
                     cycle = 2024)
    
  pred <- predict.lm(fit, data, se.fit = TRUE)
    
  data.frame(state = paste(ST),
             cycle = 2024,
             pred_vote_share = pred$fit,
             pred_se = pred$se.fit,
             pred_df = pred$df,
             pred_res = pred$residual.scale)
  })

# Clear Row Names
row.names(state_fit) <- c()

head(state_fit)

# Prior Summary Stats
modeled_EV_state_outcome_spread <- state_fit %>%
  mutate(run_date = RUN_DATE,
         spread_mu = pred_vote_share,
         # 90% Confidence Interval, T-Test Z Score
         spread_z = qt(0.95, pred_df),
         spread_t_dist = spread_z * pred_se,
         spread_start = spread_mu - spread_t_dist,
         spread_end = spread_mu + spread_t_dist,
         cycle = 2024) %>%
  select(run_date, cycle, state, spread_mu, spread_se = pred_se, spread_t_dist, spread_start, spread_end, spread_z)

head(modeled_EV_state_outcome_spread)

save(modeled_EV_state_outcome_spread, file = 'rda/modeled_EV_state_outcome_spread.rda')

write.csv(modeled_EV_state_outcome_spread, file = 'out/modeled_EV_state_outcome_spread.csv')



# Pooled Spread - CURRENTLY THE USED VALUE IN FUTURE MODELS --------------------
modeled_EV_state_spreads <- modeled_EV_state_outcome_party %>%
  group_by(state) %>%
  summarize(run_date, RUN_DATE,
            spread_mu = vote_share_mu[2] - vote_share_mu[1],
            spread_se = sqrt((vote_share_se[1]^2 + vote_share_se[2]^2)),
            spread_z = vote_share_z[1],
            spread_t_dist = spread_z * spread_se,
            spread_start = spread_mu - spread_t_dist,
            spread_end = spread_mu + spread_t_dist,
            cycle = 2024) %>%
  select(run_date, cycle, state, spread_mu, spread_se, spread_t_dist, spread_start, spread_end, spread_z)

head(modeled_EV_state_spreads)

save(modeled_EV_state_spreads, file = 'rda/modeled_EV_state_spreads.rda')

write.csv(modeled_EV_state_spreads, file = 'out/modeled_EV_state_spreads.csv')



# Correlate States to National Spread Trend ------------------------------------
# Get National Party Voter Share
dat_NAT_PARTY <- pres_results_prior %>%
  filter(state == 'National') %>%
  select(cycle, party, nat_vote_share = vote_share)

# Get State Party Voter Share
dat_STATE_PARTY <- pres_results_prior %>%
  filter(!state == 'National') %>%
  select(cycle, state, party, vote_share)

# Correlate State to Party
national_state_party_correlation <- dat_STATE_PARTY %>%
  left_join(dat_NAT_PARTY) %>%
  group_by(state, party) %>%
  summarize(national_correlation = cor(vote_share, nat_vote_share)) %>%
  mutate(run_date = RUN_DATE) %>%
  select(run_date, state, party, national_correlation)

# Visualize
dat_STATE_PARTY %>%
  left_join(dat_NAT_PARTY) %>%
#  filter(party == 'rep') %>%
  ggplot(aes(x = cycle, color = party)) +
  geom_line(aes(y = vote_share)) +
  geom_point(aes(y = vote_share)) +
  geom_line(aes(y = nat_vote_share), alpha = .5) +
  scale_color_manual(values = PARTY_COL) +
  facet_wrap(. ~ state)

# Correlation Table
national_state_party_correlation %>%
  arrange(desc(national_correlation)) %>%
  kable()

head(national_state_party_correlation)

save(national_state_party_correlation, file = 'rda/national_state_party_correlation.rda')

write.csv(national_state_party_correlation, file = 'out/national_state_party_correlation.csv')

# Join National Correlation with State Prior
modeled_EV_state_outcome_party <- modeled_EV_state_outcome_party %>%
  left_join(national_state_party_correlation) %>%
  # Assign Correlation of 1 to National
  mutate(national_correlation = ifelse(is.na(national_correlation), 1, national_correlation))

head(modeled_EV_state_outcome_party)

save(modeled_EV_state_outcome_party, file = 'rda/modeled_EV_state_outcome_party.rda')

write.csv(modeled_EV_state_outcome_party, file = 'out/modeled_EV_state_outcome_party.csv')
