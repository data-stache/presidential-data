{# Libraries
  library(lubridate)
  library(broom)
  library(knitr)
  library(dslabs)
  library(caret)
  library(radiant.data)
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
poll_history <- read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/polls/pres_pollaverages_1968-2016.csv', stringsAsFactors = FALSE, header = TRUE)

# Convert Poll Average Dates to Lubridate Format
poll_history <- poll_history %>%
  mutate(modeldate = mdy(modeldate),
         election_date = mdy(election_date)) %>%
  select(cycle, election_date, state:pct_estimate, -candidate_id)

# Clean Up State Names Final Averages
poll_history <- poll_history %>%
  mutate(state = ifelse(state == 'ME-1', 'Maine CD-1', state),
         state = ifelse(state == 'ME-2', 'Maine CD-2', state),
         state = ifelse(state == 'NE-1', 'Nebraska CD-1', state),
         state = ifelse(state == 'NE-2', 'Nebraska CD-2', state),
         state = ifelse(state == 'NE-3', 'Nebraska CD-3', state))

poll_history %>%
  group_by(cycle) %>%
  summarize(N = n())

# Cycles
CYCLES <- unique(poll_history$cycle)


# Build DF of Polling Error ----------------------------------------------------
# 2016
dat_errors <- poll_history %>%
  filter(cycle == CYCLES[1]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Donald Trump` / 100 - `Hillary Rodham Clinton` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, election_date, state, state_abb, modeldate, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

# 2012
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[2]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Mitt Romney` / 100 - `Barack Obama` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 2008
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[3]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `John McCain` / 100 - `Barack Obama` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 2004
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[4]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `George W. Bush` / 100 - `John Kerry` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 2000
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[5]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `George W. Bush` / 100 - `Al Gore` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1996
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[6]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Bob Dole` / 100 - `Bill Clinton` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1992
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[7]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `George Bush` / 100 - `Bill Clinton` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1988
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[8]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `George Bush` / 100 - `Michael S. Dukakis` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1984
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[9]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Ronald Reagan` / 100 - `Walter F. Mondale` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1980
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[10]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Ronald Reagan` / 100 - `Jimmy Carter` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1976
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[11]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Gerald R. Ford` / 100 - `Jimmy Carter` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1972
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[12]) %>%
  group_by_at(vars(-pct_estimate)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=candidate_name, value=pct_estimate) %>%    # spread
  select(-row_id) %>%  # drop the index
  mutate(D_hat = `Richard M. Nixon` / 100 - `George S. McGovern` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1968
dat_temp <- poll_history %>%
  filter(cycle == CYCLES[13]) %>%
  group_by_at(vars(-pct_estimate)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=candidate_name, value=pct_estimate) %>%    # spread
  select(-row_id) %>%  # drop the index
  mutate(D_hat = `Richard M. Nixon` / 100 - `Hubert Humphrey, Jr.` / 100,
         days_left = as.numeric(election_date - modeldate)) %>%
  select(cycle, state, modeldate, election_date, days_left, D_hat) %>%
  left_join(nat_spreads) %>%
  select(cycle, state, state_abb, modeldate, election_date, days_left, D_hat, D) %>%
  mutate(error = D_hat - D,
         called = ifelse(D > 0 & D_hat > 0 | D < 0 & D_hat < 0, TRUE, FALSE))

dat_errors <- dat_errors %>%
  rbind(dat_temp)

dat_errors <- dat_errors %>%
  rename(poll_share = D_hat, actual_share = D)

# State Error Table ------------------------------------------------------------
dat_errors %>%
  filter(days_left %in% c(250, 200, 175, 150, 125, 120, 100, 90, 80, 70, 60, 50, 40, 30, 20, 10, 0)) %>%
  group_by(days_left) %>%
  summarize(avg_error = mean(error, na.rm = TRUE),
            called = mean(called, na.rm = TRUE)) %>%
  arrange(desc(days_left)) %>%
  kable()

dat_errors %>%
#  filter(days_left %in% c(250, 200, 175, 150, 125, 120, 100, 90, 80, 70, 60, 50, 40, 30, 20, 10, 0)) %>%
  group_by(cycle, state, days_left) %>%
  summarize(avg_error = mean(error, na.rm = TRUE),
            called = mean(called, na.rm = TRUE)) %>%
  mutate(cycle = as.character(cycle)) %>%
  ggplot(aes(x = days_left, y = avg_error, color = cycle)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-1, 1, .1)) +
  geom_line() +
  facet_wrap(. ~ state)


head(dat_errors)


# State / Days Left
state_error_matrix <- dat_errors %>%
  filter(!state == 'National') %>%
  group_by(state, days_left) %>%
  summarize(mean_error = mean(error, na.rm = TRUE)) %>%
  select(days_left, state, mean_error) %>%
  spread(state, mean_error) %>%
  select(-days_left)

state_error_C_matrix <- cor(state_error_matrix, use = 'complete.obs')

library(corrplot)
corrplot(state_error_C_matrix, type = "upper", order = 'hclust')

col <- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(state_error_C_matrix, scale = 'column')



# State / Overall Left
state_error_matrix_overall <- dat_errors %>%
  filter(!state == 'National') %>%
  group_by(cycle, state) %>%
  summarize(mean_error = mean(error, na.rm = TRUE)) %>%
  spread(state, mean_error) %>%
  ungroup() %>%
  select(-cycle)

state_error_matrix_overall[is.na(state_error_matrix_overall)] <- 0

state_error_C_matrix_overall <- cor(state_error_matrix_overall, use = 'complete.obs')

library(corrplot)
corrplot(state_error_C_matrix_overall, type = "upper", order = 'hclust')

col <- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(state_error_C_matrix_overall, scale = 'column')



save(polling_error, file = 'rda/polling_error.rda')








# Visualize Polling Error ------------------------------------------------------
X_hat <- -.02 # Poll says DEMs by 2%
X <- .03 # Reality REPs by 3%

X_hat - X

vec_error_ord <- polling_error %>%
  arrange(desc(poll_avg_error)) %>%
  .$state

polling_error %>%
  mutate(state = factor(state, levels = vec_error_ord),
         start = poll_avg_error - poll_t_dist,
         end = poll_avg_error + poll_t_dist) %>%
  ggplot(aes(x = state, y = poll_avg_error, ymin = start, ymax = end, label = state)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean(polling_error$poll_avg_error - polling_error$poll_t_dist), color = 'blue', size = .5) +
  geom_hline(yintercept = mean(polling_error$poll_avg_error + polling_error$poll_t_dist), color = 'blue', size = .5) +
  geom_hline(yintercept = -.04, color = 'red') +
  geom_hline(yintercept = .04, color = 'red') +
  geom_errorbar() +
  geom_point() +
  geom_text(aes(y = start), hjust = 1, nudge_y = -.02) +
  scale_y_continuous(breaks = seq(-1,1,.02)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90))


dat_errors %>%
  left_join(polling_error) %>%
  mutate(start = poll_avg_error - poll_t_dist,
         end = poll_avg_error + poll_t_dist) %>%
  ggplot(aes(x = cycle, y = poll_avg_error)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(y = D), color = 'darkblue') +
  geom_point(aes(y = D_hat), color = 'red') +
  geom_line(aes(y = D), color = 'darkblue') +
  geom_line(aes(y = D_hat), color = 'red') +
  coord_flip() +
  scale_x_reverse() +
  facet_wrap(. ~ state)

dat_errors %>%
  left_join(polling_error) %>%
  mutate(start = poll_avg_error - poll_t_dist,
         end = poll_avg_error + poll_t_dist) %>%
  filter(state == 'Georgia') %>%
  ggplot(aes(x = cycle, y = poll_avg_error)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(y = D), color = 'darkblue') +
  geom_point(aes(y = D_hat), color = 'red') +
  geom_line(aes(y = D), color = 'darkblue') +
  geom_line(aes(y = D_hat), color = 'red') +
  coord_flip() +
  scale_x_reverse()
























