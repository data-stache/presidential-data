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
PRESIDENT <- 'Donald Trump'

# 2016 Voting history
P_REGISTERED <- .703
P_VOTED <- .614
P_REGISTERED_VOTED <- (P_REGISTERED + P_VOTED) - (P_REGISTERED * P_VOTED)

# Presidential Approval Ratings ------------------------------------------------
# Load Local Approval Ratings
ar_temp_1 <- read.csv('data/approval_ratings_all.csv', stringsAsFactors = FALSE, header = TRUE) %>%
  select(-X)

# Load FiveThirtyEight approvals
ar_temp_2 <- read.csv('https://projects.fivethirtyeight.com/trump-approval-data/approval_polllist.csv', stringsAsFactors = FALSE, header = TRUE)

# Combine
approval_ratings <- ar_temp_1 %>%
  rbind(ar_temp_2) %>%
  distinct()

# Overwrite
write.csv(approval_ratings, file = 'data/approval_ratings_all.csv')

# Reload
approval_ratings <- read.csv('data/approval_ratings_all.csv', stringsAsFactors = FALSE, header = TRUE) %>%
  select(-X) %>%
  mutate(population = factor(population, levels = c('lv', 'v', 'rv', 'a')),
         startdate = mdy(startdate),
         enddate = mdy(enddate))



# Poll Mutations ---------------------------------------------------------------
# Calculate Median Poll Count per Pollster for Poll Population Half Life
approval_ratings <- approval_ratings %>%
  # Select and rename needed columns
  select(poll_id, president, start_date = startdate, end_date = enddate, pollster, fte_grade = grade, sample_size = samplesize, population, approve, disapprove) %>%
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
  group_by(end_date, poll_id) %>%
  # Weighted Mean of Poll - Weight == Pop Penalty
  mutate(sample_size = wtd.mean(sample_size, pop_penalty, na.rm = TRUE),
         net_approve = approve - disapprove,
         approve = wtd.mean(approve, pop_penalty, na.rm = TRUE),
         disapprove = wtd.mean(disapprove, pop_penalty, na.rm = TRUE)) %>%
  # Remove duplicates
  slice_head(n = 1) %>%
  ungroup() %>%
  # Select Columns / Reorder
  select(poll_id, start_date, field_date, end_date, president, pollster, fte_grade, sample_size, population, approve, disapprove, pop_penalty) %>%
  # Arrange Descending
  arrange(desc(end_date))

# Median Count Polls for half life
MED_N_POLLS <- approval_ratings %>%
  group_by(pollster) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  summarize(med_n_polls = median(n)) %>%
  .$med_n_polls

# Weight and Clean        
approval_ratings <- approval_ratings %>%
  # Assign poll order for Population Half Life
  group_by(pollster) %>%
  arrange(desc(end_date)) %>%
  mutate(order = seq(0, n()-1, 1)) %>%
  ungroup() %>%
  # Calculate Net Approval
  mutate(net_approve = approve - disapprove,
         # Sample Half Life
         sample_half_life = sample_size * .5 ^ (order / MED_N_POLLS),
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
  select(poll_id, run_date, field_date, start_date, end_date, president, pollster, fte_grade, sample_size, sample_half_life, population, pop_penalty, approve, disapprove, net_approve, weight, weight_sample, 
         weight_recent, weight_grade)

approval_ratings %>%
  group_by(president) %>%
  summarize(r_approve_avg = mean(approve),
            r_disapprove_avg = mean(disapprove),
            r_net_approve_avg = mean(net_approve),
            w_approve_avg = wtd.mean(approve, weight, na.rm = TRUE),
            w_disapprove_avg = wtd.mean(disapprove, weight, na.rm = TRUE),
            w_net_approve_avg = wtd.mean(net_approve, weight, na.rm = TRUE))



# Visualize --------------------------------------------------------------------
TOTAL_DAYS <- as.numeric(diff(range(approval_ratings$field_date)))
SPAN <- 21 / TOTAL_DAYS

fit_approve <- loess(approve ~ as.numeric(end_date), degree = 1, span = SPAN, data = approval_ratings)
fit_disapprove <- loess(disapprove ~ as.numeric(end_date), degree = 1, span = SPAN, data = approval_ratings)
fit_net <- loess(net_approve ~ as.numeric(end_date), degree = 1, span = SPAN, data = approval_ratings)

CI_APPROVE <- approval_ratings %>%
  summarize(se_hat = sd(approve),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

CI_DISAPPROVE <- approval_ratings %>%
  summarize(se_hat = sd(disapprove),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

CI_NET <- approval_ratings %>%
  summarize(se_hat = sd(net_approve),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

approval_ratings %>%
  mutate(smooth_approve = fit_approve$fitted,
         smooth_disapprove = fit_disapprove$fitted,
         smooth_net = fit_net$fitted) %>%
  ggplot(aes(x = end_date)) +
  geom_vline(xintercept = ymd(20201104)) +
  geom_ribbon(aes(ymin = smooth_approve - CI_APPROVE, ymax = smooth_approve + CI_APPROVE), color = 'green4', alpha = .15, size = 0) +
  geom_ribbon(aes(ymin = smooth_disapprove - CI_DISAPPROVE, ymax = smooth_disapprove + CI_DISAPPROVE), color = 'red3', alpha = .15, size = 0) +
  geom_line(aes(y = smooth_approve), color = 'green4', size = 1.2) +
  geom_point(aes(y = approve), color = 'green4', alpha = .1) +
  geom_line(aes(y = smooth_disapprove), color = 'red3', size = 1.2) +
  geom_point(aes(y = disapprove), color = 'red3', alpha = .1)
  