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
pres_results_prior <- read.csv("data/potus_results_46_20_dem_rep_other.csv", stringsAsFactors = FALSE, header = TRUE) %>% 
  # Remove X Column - not sure why it shows up
  select(-X) %>%
  # Arrange by election year, then by state
  arrange(cycle, state) %>%
  # Make Party a factor for aesthetic mappings / easier maths
  mutate(party = factor(party, levels = PARTY_LEV),
         # Add a 3 Cycle half life to presidential Cycles
         cycle_weight = (CURRENT_CYCLE - cycle) / 4,
         cycle_weight = .5 ^ ((cycle_weight - 1) / 3))

# Reorder Variables
pres_results_prior <- pres_results_prior[, c(1, 12, 2:11)]

# Election Summary Spread Statistics
pres_results_prior %>%
  group_by(cycle, state) %>%
  mutate(spread = vote_share[2] - vote_share[1]) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(state) %>%
  select(cycle, cycle_weight, state, spread) %>%
  # Calculate Average Spread and Cycle Weighted Averages and SDs
  summarize(avg_spread = mean(spread, na.rm = TRUE),
            w_avg_spread = wtd.mean(spread, cycle_weight, na.rm = TRUE),
            avg_diff = w_avg_spread - avg_spread,
            sd_spread = sd(spread, na.rm = TRUE),
            w_sd_spread = sqrt(wtd.var(spread, cycle_weight, na.rm = TRUE))) %>%
  arrange(avg_diff) %>%
  kable()

# Save RDA
save(pres_results_prior, file = 'rda/pres_results_prior.rda')



# Three Party State Level Linear Models ----------------------------------------
# Unique States
STATES <- unique(pres_results_prior$state)
PARTIES <- unique(pres_results_prior$party)

# Linear Model of States Voting History
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

# Linear Model Summary Stats
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

# Save CSV and RDA
save(modeled_EV_state_outcome_party, file = 'rda/modeled_EV_state_outcome_party.rda')
write.csv(modeled_EV_state_outcome_party, file = 'out/modeled_EV_state_outcome_party.csv')



# State Level Spread Linear Models [DEPRECATED but HOLDING] --------------------
# Unique States
STATES <- unique(pres_results_prior$state)

# Spread Linear Model
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

# Prior Results Summary Stats
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


# Save CSV and RDA
save(modeled_EV_state_outcome_spread, file = 'rda/modeled_EV_state_outcome_spread.rda')
write.csv(modeled_EV_state_outcome_spread, file = 'out/modeled_EV_state_outcome_spread.csv')



# Pooled Spread - CURRENTLY THE USED VALUE IN FUTURE MODELS --------------------
modeled_EV_state_spreads <- modeled_EV_state_outcome_party %>%
  group_by(state) %>%
  summarize(run_date, RUN_DATE,
            # Calculate LM Spread
            spread_mu = vote_share_mu[2] - vote_share_mu[1],
            # Calculate Pooled SE
            spread_se = sqrt((vote_share_se[1]^2 + vote_share_se[2]^2)),
            spread_z = vote_share_z[1],
            spread_t_dist = spread_z * spread_se,
            spread_start = spread_mu - spread_t_dist,
            spread_end = spread_mu + spread_t_dist,
            cycle = 2024) %>%
  select(run_date, cycle, state, spread_mu, spread_se, spread_t_dist, spread_start, spread_end, spread_z)

# Save CSV and RDA
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

# Correlation Table
national_state_party_correlation %>%
  arrange(desc(national_correlation)) %>%
  kable()

save(national_state_party_correlation, file = 'rda/national_state_party_correlation.rda')
write.csv(national_state_party_correlation, file = 'out/national_state_party_correlation.csv')



# State Correlations Historic --------------------------------------------------
state_correlations <- pres_results_prior %>%
  filter(state != 'National') %>%
  group_by(cycle, state) %>%
  mutate(spread = vote_share[2] - vote_share[1]) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(state_abb) %>%
  select(cycle, state_abb, spread) %>%
  spread(state_abb, spread) %>%
  select(-cycle)

state_correlation_matrix <- cor(state_correlations, use = 'complete.obs')

STATES <- colnames(state_correlation_matrix)

image(state_correlation_matrix, xaxt="n", yaxt="n")
axis(1, seq(0, 1, length = length(STATES)), 
     STATES, tck=0, cex.axis=0.8)

# EXPECTED BASED ON CORRELATION - TEST
load('rda/ensemble_rigid.rda')

SPREADS <- ensemble_rigid %>%
  group_by(state) %>%
  mutate(ensemble_spread = ensemble_center[2] - ensemble_center[1]) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(state_abb) %>%
  select(state_abb, ensemble_spread) %>%
  .$ensemble_spread

names(SPREADS) <- STATES


library(MASS)
library(mvtnorm)
test <- mvrnorm(n = 40000, 
                mu = SPREADS, 
                Sigma = cov(state_correlations, use = 'complete.obs'))

apply(test, 2, mean)

mvrnorm(n = 1, 
        mu = SPREADS, 
        Sigma = cov(state_correlations, use = 'complete.obs'))

library(corrplot)
corrplot(state_correlation_matrix, type = "upper", order = 'hclust')

col <- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(state_correlation_matrix, scale = 'column')
