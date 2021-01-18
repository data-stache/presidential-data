{# Libraries
  library(Hmisc)
  library(lubridate)
  library(broom)
  library(tidyverse)
  library(tidylog)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}
options(scipen = 999)


# Master Variables -------------------------------------------------------------
RUN_DATE <- Sys.Date()

# 2016 Voting history
P_REGISTERED <- .703
P_VOTED <- .614
P_REGISTERED_VOTED <- (P_REGISTERED + P_VOTED) - (P_REGISTERED * P_VOTED)



# Additional Data --------------------------------------------------------------
# Poll Error by State
load(file = 'rda/pres_gen_poll_error.rda')



# Presidential Approval Ratings ------------------------------------------------
# Load FiveThirtyEight approvals
all_polls <- read.csv('data/president_polls.csv', stringsAsFactors = FALSE, header = TRUE) %>%
  select(-question_id, -cycle, -pollster_id, - sponsor_ids, -sponsors, -display_name, -pollster_rating_id, -office_type, -seat_number, -seat_name, -sponsor_candidate, -internal, 
         -partisan, -tracking, -nationwide_batch, -ranked_choice_reallocated, -created_at, -notes, -url, -stage, -race_id) %>%
  mutate(start_date = mdy(start_date),
         end_date = mdy(end_date),
         election_date = mdy(election_date))

all_polls$state[all_polls$state == ''] <- 'National'



# Poll Mutations ---------------------------------------------------------------
# Calculate Median Poll Count per Pollster for Poll Population Half Life
all_polls <- all_polls %>%
  # Apply population penalty
  mutate(pop_penalty = case_when(population == 'lv' ~ 1,
                                 population == 'v' ~ 1,
                                 population == 'rv' ~ P_REGISTERED_VOTED,
                                 population == 'a' ~ P_VOTED)) %>%
  rowwise() %>%
  # Find Field Date
  mutate(field_date = mean.Date(c(as.Date(start_date), as.Date(end_date)))) %>%
  ungroup() %>%
  # Group by end_date and poll_id, find duplicates
  group_by(state, end_date, poll_id, answer) %>%
  arrange(population) %>%
  # Weighted Mean of Poll - Weight == Pop Penalty
  mutate(sample_size = wtd.mean(sample_size, pop_penalty, na.rm = TRUE),
         pct = wtd.mean(pct, pop_penalty, na.rm = TRUE)) %>%
  # Remove duplicates
  slice_head(n = 1) %>%
  ungroup() %>%
  # Arrange Descending
  arrange(desc(end_date))

STATES <- unique(all_polls$state)

# Median Count Polls for half life
MED_N_POLLS <- all_polls %>%
  group_by(state, pollster) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  summarize(med_n_polls = median(n)) %>%
  .$med_n_polls

# Weight and Clean        
all_polls <- all_polls %>%
  # Assign poll order for Population Half Life
  group_by(state, pollster) %>%
  arrange(desc(end_date)) %>%
  mutate(order = seq(0, n()-1, 1)) %>%
  ungroup() %>%
  # Sample Half Life
  mutate(sample_half_life = sample_size * .5 ^ (order / MED_N_POLLS),
         # Sample Size Weight
         weight_sample = sqrt(sample_size / 600),
         # Weight Recentness
         weight_recent = .5 ^ ((as.numeric(RUN_DATE) - as.numeric(end_date)) / 14),
         # Weight Grade
         weight_grade = case_when(fte_grade == 'A+' ~ 1,
                                  fte_grade == 'A' ~ .95,
                                  fte_grade == 'A-' ~ .9,
                                  fte_grade == 'B+' ~ .88,
                                  fte_grade == 'B' ~ .85,
                                  fte_grade == 'B-' ~ .8,
                                  fte_grade == 'C+' ~ .78,
                                  fte_grade == 'C' ~ .75,
                                  fte_grade == 'C-' ~ .7,
                                  fte_grade == 'D+' ~ .68,
                                  fte_grade == 'D-' ~ .6,
                                  fte_grade == '' ~ .5,
                                  fte_grade == 'A/B' ~ .9,
                                  fte_grade == 'B/C' ~ .8,
                                  fte_grade == 'C/D' ~ .7),
         # Calculate Weight
         weight = weight_sample * weight_recent * weight_grade * pop_penalty,
         # Run Date
         run_date = RUN_DATE) %>%
  arrange(desc(end_date)) %>%
  select(poll_id, state, start_date, field_date, end_date, pollster, fte_grade, sample_size, sample_half_life, population, answer, candidate_name, candidate_party, pct, weight,
         election_date, pollster_rating_name, population_full, methodology, weight_sample, weight_recent, weight_grade, pop_penalty, run_date)

all_polls <- all_polls %>%
  left_join(pres_gen_poll_error) %>%
  mutate(state_error_mu = ifelse(is.na(state_error_mu), median(pres_gen_poll_error$state_error_mu), state_error_mu))

all_polls <- as.data.frame(all_polls)

head(all_polls)

load('rda/pres_results_prior.rda')

dat_POLLS <- all_polls %>%
  filter(answer %in% c('Biden', 'Trump')) %>%
  mutate(pct = pct / 100) %>%
  group_by(state, answer) %>%
  summarize(candidate_party = candidate_party[1],
            poll_avg = wtd.mean(pct, weight),
            poll_sd = ifelse(n() == 1, NA, sqrt(wtd.var(pct, weight, method = 'ML'))),
            N = n(),
            state_error_mu = state_error_mu[1],
            state_error_sd = state_error_sd[1],
            state_error_se = state_error_se[1]) %>%
  ungroup() %>%
  mutate(poll_sd = ifelse(is.na(poll_sd), median(poll_sd, na.rm = TRUE), poll_sd),
         poll_se = poll_sd / sqrt(N),
         adjusted_sd = sqrt((poll_sd^2 + state_error_sd^2) / 2),
         adjusted_se = adjusted_sd / sqrt(N),
         t_score = qt(0.95, N-1),
         t_score = ifelse(is.na(t_score), median(t_score, na.rm = TRUE), t_score),
         t_dist = t_score * adjusted_sd) %>%
  left_join(dat_NAT)


save(dat_POLLS, file = 'rda/dat_POLLS.rda')

ord <- dat_POLLS %>%
  arrange(spread_2020) %>%
  .$state

dat_POLLS %>%
  mutate(state = factor(state, levels = ord)) %>%
  ggplot(aes(x = state, y = spread, ymin = spread_start, ymax = spread_end)) +
  geom_hline(yintercept = 0) +
  geom_errorbar() +
  geom_point() +
  geom_point(aes(y = spread_2020), color = 'red') +
  coord_flip()

# Visualize --------------------------------------------------------------------
TOTAL_DAYS <- round(as.numeric(diff(range(all_polls$field_date[all_polls$state == 'National' & all_polls$end_date >= ymd(20200101)]))))
SPAN <- 14 / TOTAL_DAYS

fit_biden <- all_polls %>% filter(answer == 'Biden' & state == 'National' & end_date >= ymd(20200101)) %>% loess(pct ~ as.numeric(end_date), degree = 1, span = SPAN, data = .)
fit_trump <- all_polls %>% filter(answer == 'Trump' & state == 'National' & end_date >= ymd(20200101)) %>% loess(pct ~ as.numeric(end_date), degree = 1, span = SPAN, data = .)

CI_BIDEN <- all_polls %>%
  filter(answer == 'Biden' & state == 'National') %>%
  summarize(se_hat = sd(pct),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

CI_TRUMP <- all_polls %>%
  filter(answer == 'Trump' & state == 'National') %>%
  summarize(se_hat = sd(pct),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval


dat_Biden <- all_polls %>%
  filter(end_date >= ymd(20200101) & state == 'National' & answer == 'Biden') %>%
  mutate(smooth = fit_biden$fitted)

all_polls %>%
  filter(end_date >= ymd(20200101) & state == 'National' & answer == 'Trump') %>%
  mutate(smooth = fit_trump$fitted) %>%
  rbind(dat_Biden) %>%
  ggplot(aes(x = end_date, color = answer)) +
  geom_ribbon(aes(ymin = smooth - CI_BIDEN, ymax = smooth + CI_BIDEN), alpha = .15, size = 0) +
  geom_line(aes(y = smooth), size = 1.2) +
  geom_point(aes(y = pct), alpha = .1)
