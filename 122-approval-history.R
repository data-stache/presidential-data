# Load Libraries ---------------------------------------------------------------
{
library(lubridate, quietly = TRUE)
library(stringr, quietly = TRUE)
library(rvest, quietly = TRUE)
library(knitr, quietly = TRUE)
library(Hmisc , quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(tidylog, quietly = TRUE)
}



# Load Variables ---------------------------------------------------------------
RUN_DATE <- Sys.Date()

fte_grades <- read.csv('data/fte_pollster_grades_only.csv') %>%
  select(-X)

# 2016 Voting history
P_REGISTERED <- .703
P_VOTED <- .614
P_REGISTERED_VOTED <- (P_REGISTERED + P_VOTED) - (P_REGISTERED * P_VOTED)



# Obama ------------------------------------------------------------------------
# RCP Obama Approval
obama_approval <- read.csv('data/approval-history/obama-approval.csv') %>%
  # Split Start and End Dates
  separate('Date', into = c('start_date', 'end_date'), sep = ' - ') %>%
  # Split Sample size and Population
  separate('Sample', into = c('sample_size', 'population'), sep = ' ') %>%
  mutate(population = ifelse(is.na(population), sample_size, population),
         sample_size = as.numeric(sample_size),
         # If no sample size, set to 600
         sample_size = ifelse(is.na(sample_size), 600, sample_size)) %>%
  # Set Dates to Date Format
  mutate(start_date = paste(Year_S, start_date, sep = '/'),
         start_date = ymd(start_date),
         end_date = paste(Year_E, end_date, sep = '/'),
         end_date = ymd(end_date),
         # Calculate Approval Spread
         Spread = Approve - Disapprove,
         # President?
         president = 'Barack Obama',
         # FIRST innaugeration day
         innaugeration = ymd(20090120)) %>%
  rowwise() %>%
  # Set Field Date 
  mutate(field_date = mean.Date(c(as.Date(start_date), as.Date(end_date)))) %>%
  ungroup() %>%
  # Count of days in office
  mutate(days_in_office = round(as.numeric(field_date - innaugeration))) %>%
  select(president, pollster = Poll, start_date, end_date, field_date, sample_size, population, approve = Approve, disapprove = Disapprove, net_approve = Spread, innaugeration, days_in_office)

# Fix Pollster Name Consistency / Match with FTE Grades
obama_approval <- obama_approval %>%
  mutate(pollster = replace(pollster, pollster == "FOX News", "Fox News/Beacon Research/Shaw & Co. Research"),
         pollster = replace(pollster, pollster == 'Economist/YouGov', "YouGov"),
         pollster = replace(pollster, pollster == 'The Economist/YouGov', "YouGov"),
         pollster = replace(pollster, pollster == 'Reuters/Ipsos', "Ipsos"),
         pollster = replace(pollster, pollster == 'Ipsos/McClatchy', "Ipsos"),
         pollster = replace(pollster, pollster == 'Ipsos-McClatchy', "Ipsos"),
         pollster = replace(pollster, pollster == 'CBS News', "CBS News/The New York Times"),
         pollster = replace(pollster, pollster == 'CBS News/NYT', "CBS News/The New York Times"),
         pollster = replace(pollster, pollster == 'CBS/NY Times', "CBS News/The New York Times"),
         pollster = replace(pollster, pollster == 'CBS News/NY Times', "CBS News/The New York Times"),
         pollster = replace(pollster, pollster == 'CBS News/NY Times*', "CBS News/The New York Times"),
         pollster = replace(pollster, pollster == 'Rasmussen Reports', "Rasmussen Reports/Pulse Opinion Research"),
         pollster = replace(pollster, pollster == 'Rasmussen Reports*', "Rasmussen Reports/Pulse Opinion Research"),
         pollster = replace(pollster, pollster == 'Rasmussen', "Rasmussen Reports/Pulse Opinion Research"),
         pollster = replace(pollster, pollster == 'ABC News/Wash Post', "ABC News/The Washington Post"),
         pollster = replace(pollster, pollster == 'Washington Post', "ABC News/The Washington Post"),
         pollster = replace(pollster, pollster == 'Wash Post/Pew/SRBI', "Pew Research Center"),
         pollster = replace(pollster, pollster == 'CNN/ORC', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'CNN/Opinion Research', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'CNN/Opinion Research*', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'CNN/OpinionResearch', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'CNN', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'Time', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'CNN/Time', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'NBC News/Wall St. Jrnl', "NBC News/The Wall Street Journal"),
         pollster = replace(pollster, pollster == 'NBC News', "NBC News/The Wall Street Journal"),
         pollster = replace(pollster, pollster == 'Monmouth', "Monmouth University"),
         pollster = replace(pollster, pollster == 'Quinnipiac', "Quinnipiac University"),
         pollster = replace(pollster, pollster == 'Pew Research', "Pew Research Center"),
         pollster = replace(pollster, pollster == 'USA Today/Suffolk', "Suffolk University"),
         pollster = replace(pollster, pollster == 'USA Today/Pew Research', "Pew Research Center"),
         pollster = replace(pollster, pollster == 'USA Today/PSRAI', "Pew Research Center"),
         pollster = replace(pollster, pollster == 'USA Today/Gallup', "Gallup"),
         pollster = replace(pollster, pollster == 'PPP (D)', "Public Policy Polling"),
         pollster = replace(pollster, pollster == 'McClatchy/Marist', "Marist College"),
         pollster = replace(pollster, pollster == 'GWU/Battleground', "Georgetown University (Battleground)"),
         pollster = replace(pollster, pollster == 'Politico/GWU/Battleground', "Georgetown University (Battleground)"),
         pollster = replace(pollster, pollster == 'Battleground', "Georgetown University (Battleground)"),
         pollster = replace(pollster, pollster == 'Associated Press-GfK', "GfK Group"),
         pollster = replace(pollster, pollster == 'Associated Press/GfK', "GfK Group"),
         pollster = replace(pollster, pollster == 'Associated Press/CNBC', "GfK Group"),
         pollster = replace(pollster, pollster == 'NPR/GQR/Resurgent', "GQR Research (GQRR)"),
         pollster = replace(pollster, pollster == 'NPR - POS/GQR', "GQR Research (GQRR)"),
         pollster = replace(pollster, pollster == 'LA Times/USC', "USC Dornsife/Los Angeles Times"),
         pollster = replace(pollster, pollster == 'National Journal/FD', "National Journal"),
         pollster = replace(pollster, pollster == 'Schoen', "Schoen Consulting"),
         pollster = replace(pollster, pollster == 'Marist', "Marist College"),
         pollster = replace(pollster, pollster == 'Zogby', "Zogby Interactive/JZ Analytics")) %>%
  left_join(fte_grades) %>%
  mutate(fte_grade = ifelse(is.na(fte_grade), '', fte_grade))

# Recode population code to all lowercase
{
obama_approval$population[obama_approval$population == 'RV'] <- 'rv'
obama_approval$population[obama_approval$population == 'LV'] <- 'lv'
obama_approval$population[obama_approval$population == 'A'] <- 'a'
}

obama_approval <- obama_approval %>%
  # Apply population penalty
  mutate(pop_penalty = case_when(population == 'lv' ~ 1,
                                 population == 'rv' ~ P_REGISTERED_VOTED,
                                 population == 'a' ~ P_VOTED)) %>%
  # Select Columns / Reorder
  select(president, start_date, field_date, end_date, president, pollster, fte_grade, sample_size, population, pop_penalty, approve, disapprove, net_approve, innaugeration, days_in_office) %>%
  # Arrange Descending
  arrange(desc(field_date))

# Median Count Polls for half life
MED_N_POLLS <- obama_approval %>%
  group_by(pollster) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  summarize(med_n_polls = median(n)) %>%
  .$med_n_polls

obama_approval %>%
  arrange(desc(field_date)) %>%
  select(field_date, innaugeration, days_in_office) %>%
  mutate(test_1 = (as.numeric(field_date) - as.numeric(innaugeration)),
         test_2 = (as.numeric(max(field_date)) - as.numeric(field_date)),
         weight_recent = .5 ^ ((as.numeric(max(field_date)) - as.numeric(field_date)) / 14))

# Weight and Clean        
obama_approval <- obama_approval %>%
  # Assign poll order for Population Half Life
  group_by(pollster) %>%
  arrange(desc(field_date)) %>%
  mutate(order = seq(0, n()-1, 1)) %>%
  ungroup() %>%
  # Sample Half Life
  mutate(sample_half_life = sample_size * .5 ^ (order / MED_N_POLLS),
         # Sample Size Weight
         weight_sample = sqrt(sample_size / 600),
         # Weight Recentness
         weight_recent = .5 ^ ((as.numeric(max(field_date)) - as.numeric(field_date)) / 14),
         # FTE Weight Grade
         weight_grade = case_when(fte_grade == 'A+' ~ 1,
                                  fte_grade == 'A' ~ .95,
                                  fte_grade == 'A-' ~ .9,
                                  fte_grade == 'A/B' ~ .9,
                                  fte_grade == 'B+' ~ .88,
                                  fte_grade == 'B' ~ .85,
                                  fte_grade == 'B-' ~ .8,
                                  fte_grade == 'B/C' ~ .8,
                                  fte_grade == 'C+' ~ .78,
                                  fte_grade == 'C' ~ .75,
                                  fte_grade == 'C-' ~ .7,
                                  fte_grade == 'C/D' ~ .7,
                                  fte_grade == 'D+' ~ .68,
                                  fte_grade == 'D-' ~ .6,
                                  fte_grade == '' ~ .5),
         # Calculate Weights
         weight_election = weight_sample * weight_recent * weight_grade * pop_penalty,
         weight_compare = weight_sample * weight_recent * weight_grade,
         # Run Date
         run_date = RUN_DATE) %>%
  arrange(desc(field_date)) %>%
  select(run_date, field_date, start_date, end_date, president, pollster, fte_grade, sample_size, sample_half_life, population, pop_penalty, approve, disapprove, net_approve,
         weight_election, weight_compare, innaugeration, days_in_office, weight_sample, weight_recent, weight_grade)

obama_approval %>%
  group_by(president) %>%
  dplyr::summarize(r_approve_avg = mean(approve),
            r_disapprove_avg = mean(disapprove),
            r_net_approve_avg = mean(net_approve),
            e_approve_avg = wtd.mean(approve, weight_election, na.rm = TRUE),
            e_disapprove_avg = wtd.mean(disapprove, weight_election, na.rm = TRUE),
            e_net_approve_avg = wtd.mean(net_approve, weight_election, na.rm = TRUE),
            w_approve_avg = wtd.mean(approve, weight_compare, na.rm = TRUE),
            w_disapprove_avg = wtd.mean(disapprove, weight_compare, na.rm = TRUE),
            w_net_approve_avg = wtd.mean(net_approve, weight_compare, na.rm = TRUE))



# Smooth -----------------------------------------------------------------------
TOTAL_DAYS <- as.numeric(diff(range(obama_approval$field_date)))
SPAN <- 28 / TOTAL_DAYS

fit_approve <- loess(approve ~ as.numeric(field_date), degree = 1, span = SPAN, data = obama_approval)
fit_disapprove <- loess(disapprove ~ as.numeric(field_date), degree = 1, span = SPAN, data = obama_approval)
fit_net <- loess(net_approve ~ as.numeric(field_date), degree = 1, span = SPAN, data = obama_approval)

obama_approval_smoothed <- obama_approval %>%
  mutate(smooth_approve = fit_approve$fitted,
         smooth_disapprove = fit_disapprove$fitted) %>%
  distinct(president, field_date, smooth_approve, smooth_disapprove)



# Visualize --------------------------------------------------------------------
CI_APPROVE <- obama_approval %>%
  dplyr::summarize(se_hat = sd(approve),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

CI_DISAPPROVE <- obama_approval %>%
  dplyr::summarize(se_hat = sd(disapprove),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

CI_NET <- obama_approval %>%
  dplyr::summarize(se_hat = sd(net_approve),
            interval = qnorm(0.95) * se_hat) %>%
  .$interval

obama_approval %>%
  mutate(smooth_approve = fit_approve$fitted,
         smooth_disapprove = fit_disapprove$fitted,
         smooth_net = fit_net$fitted) %>%
  ggplot(aes(x = field_date)) +
  geom_vline(xintercept = ymd(20201104)) +
  geom_ribbon(aes(ymin = smooth_approve - CI_APPROVE, ymax = smooth_approve + CI_APPROVE), color = 'green4', alpha = .15, size = 0) +
  geom_ribbon(aes(ymin = smooth_disapprove - CI_DISAPPROVE, ymax = smooth_disapprove + CI_DISAPPROVE), color = 'red3', alpha = .15, size = 0) +
  geom_line(aes(y = smooth_approve), color = 'green4', size = 1.2) +
  geom_point(aes(y = approve), color = 'green4', alpha = .1) +
  geom_line(aes(y = smooth_disapprove), color = 'red3', size = 1.2) +
  geom_point(aes(y = disapprove), color = 'red3', alpha = .1)



# Bush -------------------------------------------------------------------------
# RCP Obama Approval
bush_approval <- read.csv('data/approval-history/obama-approval.csv') %>%
  # Split Start and End Dates
  separate('Date', into = c('start_date', 'end_date'), sep = ' - ') %>%
  # Split Sample size and Population
  separate('Sample', into = c('sample_size', 'population'), sep = ' ') %>%
  mutate(population = ifelse(is.na(population), sample_size, population),
         sample_size = as.numeric(sample_size),
         # If no sample size, set to 600
         sample_size = ifelse(is.na(sample_size), 600, sample_size)) %>%
  # Set Dates to Date Format
  mutate(start_date = paste(Year_S, start_date, sep = '/'),
         start_date = ymd(start_date),
         end_date = paste(Year_E, end_date, sep = '/'),
         end_date = ymd(end_date),
         # Calculate Approval Spread
         Spread = Approve - Disapprove,
         # President?
         president = 'Barack Obama',
         # FIRST innaugeration day
         innaugeration = ymd(20090120)) %>%
  rowwise() %>%
  # Set Field Date 
  mutate(field_date = mean.Date(c(as.Date(start_date), as.Date(end_date)))) %>%
  ungroup() %>%
  # Count of days in office
  mutate(days_in_office = round(as.numeric(field_date - innaugeration))) %>%
  select(president, pollster = Poll, start_date, end_date, field_date, sample_size, population, approve = Approve, disapprove = Disapprove, net_approve = Spread, innaugeration, days_in_office)

# Fix Pollster Name Consistency / Match with FTE Grades
obama_approval <- obama_approval %>%
  mutate(pollster = replace(pollster, pollster == "FOX News", "Fox News/Beacon Research/Shaw & Co. Research"),
         pollster = replace(pollster, pollster == 'Economist/YouGov', "YouGov"),
         pollster = replace(pollster, pollster == 'The Economist/YouGov', "YouGov"),
         pollster = replace(pollster, pollster == 'Reuters/Ipsos', "Ipsos"),
         pollster = replace(pollster, pollster == 'Ipsos/McClatchy', "Ipsos"),
         pollster = replace(pollster, pollster == 'Ipsos-McClatchy', "Ipsos"),
         pollster = replace(pollster, pollster == 'CBS News', "CBS News/The New York Times"),
         pollster = replace(pollster, pollster == 'CBS News/NYT', "CBS News/The New York Times"),
         pollster = replace(pollster, pollster == 'CBS/NY Times', "CBS News/The New York Times"),
         pollster = replace(pollster, pollster == 'CBS News/NY Times', "CBS News/The New York Times"),
         pollster = replace(pollster, pollster == 'CBS News/NY Times*', "CBS News/The New York Times"),
         pollster = replace(pollster, pollster == 'Rasmussen Reports', "Rasmussen Reports/Pulse Opinion Research"),
         pollster = replace(pollster, pollster == 'Rasmussen Reports*', "Rasmussen Reports/Pulse Opinion Research"),
         pollster = replace(pollster, pollster == 'Rasmussen', "Rasmussen Reports/Pulse Opinion Research"),
         pollster = replace(pollster, pollster == 'ABC News/Wash Post', "ABC News/The Washington Post"),
         pollster = replace(pollster, pollster == 'Washington Post', "ABC News/The Washington Post"),
         pollster = replace(pollster, pollster == 'Wash Post/Pew/SRBI', "Pew Research Center"),
         pollster = replace(pollster, pollster == 'CNN/ORC', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'CNN/Opinion Research', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'CNN/Opinion Research*', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'CNN/OpinionResearch', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'CNN', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'Time', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'CNN/Time', "CNN/Opinion Research Corp."),
         pollster = replace(pollster, pollster == 'NBC News/Wall St. Jrnl', "NBC News/The Wall Street Journal"),
         pollster = replace(pollster, pollster == 'NBC News', "NBC News/The Wall Street Journal"),
         pollster = replace(pollster, pollster == 'Monmouth', "Monmouth University"),
         pollster = replace(pollster, pollster == 'Quinnipiac', "Quinnipiac University"),
         pollster = replace(pollster, pollster == 'Pew Research', "Pew Research Center"),
         pollster = replace(pollster, pollster == 'USA Today/Suffolk', "Suffolk University"),
         pollster = replace(pollster, pollster == 'USA Today/Pew Research', "Pew Research Center"),
         pollster = replace(pollster, pollster == 'USA Today/PSRAI', "Pew Research Center"),
         pollster = replace(pollster, pollster == 'USA Today/Gallup', "Gallup"),
         pollster = replace(pollster, pollster == 'PPP (D)', "Public Policy Polling"),
         pollster = replace(pollster, pollster == 'McClatchy/Marist', "Marist College"),
         pollster = replace(pollster, pollster == 'GWU/Battleground', "Georgetown University (Battleground)"),
         pollster = replace(pollster, pollster == 'Politico/GWU/Battleground', "Georgetown University (Battleground)"),
         pollster = replace(pollster, pollster == 'Battleground', "Georgetown University (Battleground)"),
         pollster = replace(pollster, pollster == 'Associated Press-GfK', "GfK Group"),
         pollster = replace(pollster, pollster == 'Associated Press/GfK', "GfK Group"),
         pollster = replace(pollster, pollster == 'Associated Press/CNBC', "GfK Group"),
         pollster = replace(pollster, pollster == 'NPR/GQR/Resurgent', "GQR Research (GQRR)"),
         pollster = replace(pollster, pollster == 'NPR - POS/GQR', "GQR Research (GQRR)"),
         pollster = replace(pollster, pollster == 'LA Times/USC', "USC Dornsife/Los Angeles Times"),
         pollster = replace(pollster, pollster == 'National Journal/FD', "National Journal"),
         pollster = replace(pollster, pollster == 'Schoen', "Schoen Consulting"),
         pollster = replace(pollster, pollster == 'Marist', "Marist College"),
         pollster = replace(pollster, pollster == 'Zogby', "Zogby Interactive/JZ Analytics")) %>%
  left_join(fte_grades) %>%
  mutate(fte_grade = ifelse(is.na(fte_grade), '', fte_grade))

# Recode population code to all lowercase
{
  obama_approval$population[obama_approval$population == 'RV'] <- 'rv'
  obama_approval$population[obama_approval$population == 'LV'] <- 'lv'
  obama_approval$population[obama_approval$population == 'A'] <- 'a'
  }

obama_approval <- obama_approval %>%
  # Apply population penalty
  mutate(pop_penalty = case_when(population == 'lv' ~ 1,
                                 population == 'rv' ~ P_REGISTERED_VOTED,
                                 population == 'a' ~ P_VOTED)) %>%
  # Select Columns / Reorder
  select(president, start_date, field_date, end_date, president, pollster, fte_grade, sample_size, population, pop_penalty, approve, disapprove, net_approve, innaugeration, days_in_office) %>%
  # Arrange Descending
  arrange(desc(field_date))

# Median Count Polls for half life
MED_N_POLLS <- obama_approval %>%
  group_by(pollster) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  summarize(med_n_polls = median(n)) %>%
  .$med_n_polls

obama_approval %>%
  arrange(desc(field_date)) %>%
  select(field_date, innaugeration, days_in_office) %>%
  mutate(test_1 = (as.numeric(field_date) - as.numeric(innaugeration)),
         test_2 = (as.numeric(max(field_date)) - as.numeric(field_date)),
         weight_recent = .5 ^ ((as.numeric(max(field_date)) - as.numeric(field_date)) / 14))

# Weight and Clean        
obama_approval <- obama_approval %>%
  # Assign poll order for Population Half Life
  group_by(pollster) %>%
  arrange(desc(field_date)) %>%
  mutate(order = seq(0, n()-1, 1)) %>%
  ungroup() %>%
  # Sample Half Life
  mutate(sample_half_life = sample_size * .5 ^ (order / MED_N_POLLS),
         # Sample Size Weight
         weight_sample = sqrt(sample_size / 600),
         # Weight Recentness
         weight_recent = .5 ^ ((as.numeric(max(field_date)) - as.numeric(field_date)) / 14),
         # FTE Weight Grade
         weight_grade = case_when(fte_grade == 'A+' ~ 1,
                                  fte_grade == 'A' ~ .95,
                                  fte_grade == 'A-' ~ .9,
                                  fte_grade == 'A/B' ~ .9,
                                  fte_grade == 'B+' ~ .88,
                                  fte_grade == 'B' ~ .85,
                                  fte_grade == 'B-' ~ .8,
                                  fte_grade == 'B/C' ~ .8,
                                  fte_grade == 'C+' ~ .78,
                                  fte_grade == 'C' ~ .75,
                                  fte_grade == 'C-' ~ .7,
                                  fte_grade == 'C/D' ~ .7,
                                  fte_grade == 'D+' ~ .68,
                                  fte_grade == 'D-' ~ .6,
                                  fte_grade == '' ~ .5),
         # Calculate Weights
         weight_election = weight_sample * weight_recent * weight_grade * pop_penalty,
         weight_compare = weight_sample * weight_recent * weight_grade,
         # Run Date
         run_date = RUN_DATE) %>%
  arrange(desc(field_date)) %>%
  select(run_date, field_date, start_date, end_date, president, pollster, fte_grade, sample_size, sample_half_life, population, pop_penalty, approve, disapprove, net_approve,
         weight_election, weight_compare, innaugeration, days_in_office, weight_sample, weight_recent, weight_grade)

obama_approval %>%
  group_by(president) %>%
  dplyr::summarize(r_approve_avg = mean(approve),
                   r_disapprove_avg = mean(disapprove),
                   r_net_approve_avg = mean(net_approve),
                   e_approve_avg = wtd.mean(approve, weight_election, na.rm = TRUE),
                   e_disapprove_avg = wtd.mean(disapprove, weight_election, na.rm = TRUE),
                   e_net_approve_avg = wtd.mean(net_approve, weight_election, na.rm = TRUE),
                   w_approve_avg = wtd.mean(approve, weight_compare, na.rm = TRUE),
                   w_disapprove_avg = wtd.mean(disapprove, weight_compare, na.rm = TRUE),
                   w_net_approve_avg = wtd.mean(net_approve, weight_compare, na.rm = TRUE))



# Smooth -----------------------------------------------------------------------
TOTAL_DAYS <- as.numeric(diff(range(obama_approval$field_date)))
SPAN <- 28 / TOTAL_DAYS

fit_approve <- loess(approve ~ as.numeric(field_date), degree = 1, span = SPAN, data = obama_approval)
fit_disapprove <- loess(disapprove ~ as.numeric(field_date), degree = 1, span = SPAN, data = obama_approval)
fit_net <- loess(net_approve ~ as.numeric(field_date), degree = 1, span = SPAN, data = obama_approval)

obama_approval_smoothed <- obama_approval %>%
  mutate(smooth_approve = fit_approve$fitted,
         smooth_disapprove = fit_disapprove$fitted) %>%
  distinct(president, field_date, smooth_approve, smooth_disapprove)



# Visualize --------------------------------------------------------------------
CI_APPROVE <- obama_approval %>%
  dplyr::summarize(se_hat = sd(approve),
                   interval = qnorm(0.95) * se_hat) %>%
  .$interval

CI_DISAPPROVE <- obama_approval %>%
  dplyr::summarize(se_hat = sd(disapprove),
                   interval = qnorm(0.95) * se_hat) %>%
  .$interval

CI_NET <- obama_approval %>%
  dplyr::summarize(se_hat = sd(net_approve),
                   interval = qnorm(0.95) * se_hat) %>%
  .$interval

obama_approval %>%
  mutate(smooth_approve = fit_approve$fitted,
         smooth_disapprove = fit_disapprove$fitted,
         smooth_net = fit_net$fitted) %>%
  ggplot(aes(x = field_date)) +
  geom_vline(xintercept = ymd(20201104)) +
  geom_ribbon(aes(ymin = smooth_approve - CI_APPROVE, ymax = smooth_approve + CI_APPROVE), color = 'green4', alpha = .15, size = 0) +
  geom_ribbon(aes(ymin = smooth_disapprove - CI_DISAPPROVE, ymax = smooth_disapprove + CI_DISAPPROVE), color = 'red3', alpha = .15, size = 0) +
  geom_line(aes(y = smooth_approve), color = 'green4', size = 1.2) +
  geom_point(aes(y = approve), color = 'green4', alpha = .1) +
  geom_line(aes(y = smooth_disapprove), color = 'red3', size = 1.2) +
  geom_point(aes(y = disapprove), color = 'red3', alpha = .1)




























