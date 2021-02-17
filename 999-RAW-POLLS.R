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

# Read Pertinent Files ---------------------------------------------------------
load('rda/modeled_EV_state_spreads.rda')
head(modeled_EV_state_spreads)
nat_spreads <- read.csv('data/potus_results_46_20_dem_rep_other.csv', stringsAsFactors = FALSE, header = TRUE) %>%
  select(-X) %>%
  group_by(cycle, state) %>%
  mutate(D = vote_share[2] - vote_share[1]) %>%
  slice_head(n = 1) %>%
  select(cycle, state, state_abb, D, state_winner)

head(nat_spreads)

# Master Variables -------------------------------------------------------------
RUN_DATE <- Sys.Date()
CURRENT_CYCLE <- 2024

# Graph Colors
PARTY_COL <- c('darkblue', 'red', 'green4')
PARTY_LEV <- c('dem', 'rep', 'other')




# Load Poll History ------------------------------------------------------------
# Load
poll_history <- read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw-polls.csv', stringsAsFactors = FALSE, header = TRUE)

tail(poll_history)

















