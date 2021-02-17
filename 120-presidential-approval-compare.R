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
PRESIDENT <- 'Joseph R. Biden Jr.'



# Presidential Approval Ratings ------------------------------------------------
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
  # Find Field Date
  rowwise() %>%
  mutate(field_date = mean.Date(c(start_date, end_date))) %>%
  ungroup() %>%
  # Group by end_date and poll_id, find duplicates
  group_by(end_date, poll_id) %>%
  # Weighted Mean of Poll - Weight == Pop Penalty
  mutate(net_approve = approve - disapprove) %>%
  # Remove duplicates
  slice_head(n = 1) %>%
  ungroup() %>%
  # Select Columns / Reorder
  select(poll_id, start_date, field_date, end_date, president, pollster, fte_grade, sample_size, population, net_approve, approve, disapprove) %>%
  # Arrange Descending
  arrange(desc(field_date))

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
         weight_recent = .5 ^ ((as.numeric(RUN_DATE) - as.numeric(field_date)) / 14),
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
         weight = weight_sample * weight_recent * weight_grade,
         # Run Date
         run_date = RUN_DATE) %>%
  arrange(desc(end_date)) %>%
  select(run_date, field_date, start_date, end_date, president, pollster, fte_grade, sample_size, sample_half_life, population, approve, disapprove, net_approve, weight, weight_sample, 
         weight_recent, weight_grade)

approval_ratings %>%
  group_by(president) %>%
  summarize(r_approve_avg = mean(approve),
            r_disapprove_avg = mean(disapprove),
            r_net_approve_avg = mean(net_approve),
            w_approve_avg = wtd.mean(approve, weight, na.rm = TRUE),
            w_disapprove_avg = wtd.mean(disapprove, weight, na.rm = TRUE),
            w_net_approve_avg = wtd.mean(net_approve, weight, na.rm = TRUE))



# JOSEPH R. BIDEN JR APPROVAL --------------------------------------------------
pres <- 'Joseph R. Biden Jr.'
TOTAL_DAYS <- as.numeric(diff(range(approval_ratings$field_date[approval_ratings$president == pres])))
SPAN <- 21 / TOTAL_DAYS

fit_approve_biden <- approval_ratings %>%
  filter(president == pres) %>%
  loess(approve ~ as.numeric(field_date), degree = 1, span = SPAN, data = .)

fit_disapprove_biden <- approval_ratings %>%
  filter(president == pres) %>%
  loess(disapprove ~ as.numeric(field_date), degree = 1, span = SPAN, data = .)

fit_net_biden <- approval_ratings %>%
  filter(president == pres) %>%
  loess(net_approve ~ as.numeric(field_date), degree = 1, span = SPAN, data = .)

approval_smoothed <- approval_ratings %>%
  filter(president == pres) %>%
  mutate(smooth_approve = fit_approve_biden$fitted,
         smooth_disapprove = fit_disapprove_biden$fitted) %>%
  distinct(president, field_date, smooth_approve, smooth_disapprove)



# Visualize 
CI_APPROVE_BIDEN <- approval_ratings %>%
  filter(president == pres) %>%
  summarize(se_hat = sd(approve),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

CI_DISAPPROVE_BIDEN <- approval_ratings %>%
  filter(president == pres) %>%
  summarize(se_hat = sd(disapprove),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

CI_NET_BIDEN <- approval_ratings %>%
  filter(president == pres) %>%
  summarize(se_hat = sd(net_approve),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

approval_ratings %>%
  filter(president == pres) %>%
  mutate(smooth_approve = fit_approve_biden$fitted,
         smooth_disapprove = fit_disapprove_biden$fitted,
         smooth_net = fit_net_biden$fitted) %>%
  ggplot(aes(x = field_date)) +
  geom_hline(yintercept = 50, color = 'grey40') +
  geom_ribbon(aes(ymin = smooth_approve - CI_APPROVE_BIDEN, ymax = smooth_approve + CI_APPROVE_BIDEN), color = 'green4', alpha = .15, size = 0) +
  geom_ribbon(aes(ymin = smooth_disapprove - CI_DISAPPROVE_BIDEN, ymax = smooth_disapprove + CI_DISAPPROVE_BIDEN), color = 'red3', alpha = .15, size = 0) +
  geom_line(aes(y = smooth_approve), color = 'green4', size = 1.2) +
  geom_point(aes(y = approve), color = 'green4', alpha = .1) +
  geom_line(aes(y = smooth_disapprove), color = 'red3', size = 1.2) +
  geom_point(aes(y = disapprove), color = 'red3', alpha = .1)


if (FALSE) {
# DONALD TRUMP APPROVAL --------------------------------------------------------
pres <- 'Donald Trump'
TOTAL_DAYS <- as.numeric(diff(range(approval_ratings$field_date[approval_ratings$president == pres])))
SPAN <- 21 / TOTAL_DAYS

fit_approve_trump <- approval_ratings %>%
  filter(president == pres) %>%
  loess(approve ~ as.numeric(field_date), degree = 1, span = SPAN, data = .)

fit_disapprove_trump <- approval_ratings %>%
  filter(president == pres) %>%
  loess(disapprove ~ as.numeric(field_date), degree = 1, span = SPAN, data = .)

fit_net_trump <- approval_ratings %>%
  filter(president == pres) %>%
  loess(net_approve ~ as.numeric(field_date), degree = 1, span = SPAN, data = .)

approval_smoothed <- approval_ratings %>%
  filter(president == pres) %>%
  mutate(smooth_approve = fit_approve_trump$fitted,
         smooth_disapprove = fit_disapprove_trump$fitted) %>%
  distinct(president, field_date, smooth_approve, smooth_disapprove)



# Visualize 
CI_APPROVE_TRUMP <- approval_ratings %>%
  filter(president == pres) %>%
  summarize(se_hat = sd(approve),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

CI_DISAPPROVE_TRUMP <- approval_ratings %>%
  filter(president == pres) %>%
  summarize(se_hat = sd(disapprove),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

CI_NET_TRUMP <- approval_ratings %>%
  filter(president == pres) %>%
  summarize(se_hat = sd(net_approve),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

approval_ratings %>%
  filter(president == pres) %>%
  mutate(smooth_approve = fit_approve_trump$fitted,
         smooth_disapprove = fit_disapprove_trump$fitted,
         smooth_net = fit_net_trump$fitted) %>%
  ggplot(aes(x = field_date)) +
  geom_hline(yintercept = 50, color = 'grey40') +
  geom_ribbon(aes(ymin = smooth_approve - CI_APPROVE_TRUMP, ymax = smooth_approve + CI_APPROVE_TRUMP), color = 'green4', alpha = .15, size = 0) +
  geom_ribbon(aes(ymin = smooth_disapprove - CI_DISAPPROVE_TRUMP, ymax = smooth_disapprove + CI_DISAPPROVE_TRUMP), color = 'red3', alpha = .15, size = 0) +
  geom_line(aes(y = smooth_approve), color = 'green4', size = 1.2) +
  geom_point(aes(y = approve), color = 'green4', alpha = .1) +
  geom_line(aes(y = smooth_disapprove), color = 'red3', size = 1.2) +
  geom_point(aes(y = disapprove), color = 'red3', alpha = .1)
}
