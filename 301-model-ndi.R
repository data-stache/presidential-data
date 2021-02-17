{# Libraries
  library(tidyverse)
  library(tidylog)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}
options(scipen = 999)

# Load Data --------------------------------------------------------------------
load('rda/expected_NDI.rda')

head(expected_NDI)

NDI <- expected_NDI %>%
  group_by(state) %>%
  mutate(NDI_spread = expected_NDI[2] - expected_NDI[1]) %>%
  slice_head(n = 1) %>%
  select(state, state_abb, NDI_spread)

states <- expected_NDI %>%
  filter(!state == 'National') %>%
  ungroup() %>%
  distinct(state, state_abb)

NDI <- read.csv('data/electoral-votes.csv') %>%
  select(-X) %>%
  left_join(states) %>%
  left_join(NDI) %>%
  select(state, state_abb, ev, NDI_spread)

load('rda/modeled_EV_state_outcome_party.rda')
head(modeled_EV_state_outcome_party)

NATIONAL <- modeled_EV_state_outcome_party %>%
  filter(state == 'National') %>%
  mutate(spread = vote_share_mu[2] - vote_share_mu[1]) %>%
  slice_head(n = 1) %>%
  .$spread

# If not using Polls
NATIONAL <- -.0337 # Set to desired spread

NDI <- NDI %>%
  mutate(nat_mu = NATIONAL,
         spread_mu = NDI_spread + nat_mu) %>%
  select(-NDI_spread, -nat_mu)



# Functions --------------------------------------------------------------------
t_draws <- function (N, MU = 0, SD = 1, DF = 10) {
  rnorm(n = N, mean = MU, sd = SD) / sqrt(rchisq(n = N, df = DF) / DF)
}



# Variables --------------------------------------------------------------------
# Set Variables
B <- 40000 # Iterations
N <- length(states$state) # Count of States (plus National)
D <- length(seq(1948, 2020, 4)) - 1



# Rigid Model ------------------------------------------------------------------
# Build Empty Results Matrix
results_matrix <- matrix(0, nrow = N, ncol = B)

# Build list for T Distribution Curve
conf_int <- t_draws(B, SD = .025, DF = D)
surprises <- replicate(B, {
  sample(c(0, t_draws(1, SD = .07, DF = D)), 1, replace = TRUE, prob = c(.95, .05))
})

# Run Simulation
for(i in 1:N){
  results_matrix[i, ] <- NDI[i, 4] + conf_int
}

# P of rep Wins
rep_wins <- ifelse(results_matrix > 0, 1, 0)
rep_state_probs <- apply(rep_wins, 1, sum)/B

# Attach to Posteriors Table
NDI$rep_prob <- rep_state_probs

# P of dem Wins
dem_wins <- ifelse(results_matrix < 0, 1, 0)
dem_state_probs <- apply(dem_wins, 1, sum)/B

# Attach to Posteriors Table
NDI$dem_prob <- dem_state_probs

# Make EV Matrix
votes <- matrix(NDI$ev, ncol = 1)

# Count Electoral Votes
for(i in 1:B){
  rep_wins[ ,i] <- rep_wins[ ,i] * votes
  dem_wins[ ,i] <- dem_wins[ ,i] * votes
}

# Sum Electoral Votes
rep_votes <- apply(rep_wins, 2, sum, na.rm = TRUE)
dem_votes <- apply(dem_wins, 2, sum, na.rm = TRUE)

# Results Data Frame
results <- data.frame(rep_votes, dem_votes)

results %>%
  select(-winner) %>%
  rename(rep = rep_votes, dem = dem_votes) %>%
  gather(party, votes) %>%
  ggplot() +
  geom_vline(xintercept = 270) +
  geom_histogram(aes(x = votes, fill = party), position = 'dodge', binwidth = 5) +
  scale_x_continuous(breaks = seq(0, 540, 10)) +
  scale_fill_manual(values = c('blue', 'red'))

# Label Winner
results <- results %>% 
  mutate(winner = ifelse(rep_votes >= dem_votes, "rep", "dem"))

# Build this out - National vote / Electoral vote / Ties etc

# Build a table of wins
win_table <- results %>% 
  group_by(winner) %>% 
  tally() %>% 
  rename(percent = n) %>% 
  mutate(percent = percent/B)

# Display table of wins
results %>% 
  group_by(winner) %>% 
  tally() %>% 
  rename(Percent = n) %>%   
  mutate(Percent = scales::percent(Percent/B, accuracy = 1.1))

NDI 

