{# Libraries
  library(tidyverse)
  library(tidylog)
  library(lubridate)
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

# State Presidential Election Data ---------------------------------------------
# Load Election Results since 1976
approval_ratings <- read.csv('https://projects.fivethirtyeight.com/trump-approval-data/approval_polllist.csv', stringsAsFactors = FALSE, header = TRUE) %>%
  mutate(startdate = mdy(startdate),
         enddate = mdy(enddate))

# Calculate Median Poll Count per Pollster for Poll Population Half Life
MED_N_POLLS <- approval_ratings %>%
  group_by(pollster) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  summarize(med_n_polls = median(n)) %>%
  .$med_n_polls

approval_ratings <- approval_ratings %>%
  select(poll_id, president, start_date = startdate, end_date = enddate, pollster, fte_grade = grade, sample_size = samplesize, population, approve, disapprove) %>%
  group_by(pollster) %>%
  arrange(desc(end_date)) %>%
  mutate(order = seq(0, n()-1, 1),
         sample_half_life = sample_size * .5 ^ (order / MED_N_POLLS),
         weight_sample = sqrt(sample_size / 600),
         weight_recent = .5 ^ ((as.numeric(RUN_DATE) - as.numeric(end_date)) / 14),
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
         pop_penalty = case_when(population == 'lv' ~ 1,
                                 population == 'v' ~ 1,
                                 population == 'rv' ~ P_REGISTERED_VOTED,
                                 population == 'a' ~ P_VOTED),
         weight = weight_sample * weight_recent * weight_grade * pop_penalty,
         run_date = RUN_DATE) %>%
  arrange(desc(end_date)) %>%
  select(run_date, poll_id, president, start_date, end_date, pollster, fte_grade, sample_size, sample_half_life, population, approve, disapprove, weight_sample, weight_recent, weight_grade,
         pop_penalty, weight)

head(approval_ratings)

approval_ratings %>%
  group_by(president) %>%
  summarize(approve_avg = weighted.mean(approve, weight, na.rm = TRUE),
           disapprove_avg = weighted.mean(disapprove, weight, na.rm = TRUE))

approval_ratings %>%
  ggplot(aes(x = end_date)) +
  geom_point(aes(y = approve), color = 'green4', alpha = .3) +
  geom_point(aes(y = disapprove), color = 'red4', alpha = .3) +
  geom_smooth(aes(y = approve), color = 'green4', method.args = list(degree=1)) +
  geom_smooth(aes(y = disapprove), color = 'red4', span = 0.05) 
geom_smo  
  