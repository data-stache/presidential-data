{
  # LOAD LIBRARIES
  library(tidyverse)
  library(lubridate)
  library(gridExtra)
  library(zoo)
  library(tidyquant)
  library(fredr)
  library(tidylog)
  library(tidyquant)
  library(alfred)
  library(caret)
}
options(digits = 5)

# Set FRED API key
fred_api_key <- '1f1b7e461c5ff48e62f4f94436f38105'
fredr_set_key(fred_api_key)

# Functions
# Percent_Change
per_change <- function(x, na.rm = TRUE) ((x - lead(x)) / lead(x))

# Normalize Data
z_norm <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)



# Unemployment -----------------------------------------------------------------
# Pull Data From Alfred
dat_UNRATE <- get_alfred_series('UNRATE', 'value') %>%
  drop_na() %>%
  arrange(date) %>%
  group_by(date) %>%
  # Select First Observation
  slice(1) %>%
  ungroup() %>%
  arrange(desc(date)) 

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:12
name_vec <- paste(prefix, suffix, sep = '')
dat_UNRATE[, name_vec] <- NA

# Roll apply rolling averages (1 month, 2 month...n)
for(i in seq_along(name_vec)) {
  dat_UNRATE[, 3 + i] <- rollapply(dat_UNRATE$value, width = i, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
}

dat_UNRATE <- dat_UNRATE %>%
  # Normalize Columns
  mutate_at(name_vec, per_change) %>%
  mutate_at(name_vec, z_norm) %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         var = 'UNRATE') %>%
  select(date, realtime_period, value, var, score) %>%
  drop_na()

head(dat_UNRATE)



# Non Farm Payroll -------------------------------------------------------------
# Pull Data From Alfred
dat_PAYEMS <- get_alfred_series('PAYEMS', 'value') %>%
  drop_na() %>%
  arrange(date) %>%
  group_by(date) %>%
  # Select First Observation
  slice(1) %>%
  ungroup() %>%
  arrange(desc(date)) 

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:12
name_vec <- paste(prefix, suffix, sep = '')
dat_PAYEMS[, name_vec] <- NA

# Roll apply rolling averages (1 month, 2 month...n)
for(i in seq_along(name_vec)) {
  dat_PAYEMS[, 3 + i] <- rollapply(dat_PAYEMS$value, width = i, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
}

dat_PAYEMS <- dat_PAYEMS %>%
  # Normalize Columns
  mutate_at(name_vec, per_change) %>%
  mutate_at(name_vec, z_norm) %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         var = 'PAYEMS') %>%
  select(date, realtime_period, value, var, score) %>%
  drop_na()

head(dat_PAYEMS)



# Personal Income --------------------------------------------------------------
# Pull Data From Alfred
dat_PI <- get_alfred_series('PI', 'value') %>%
  drop_na() %>%
  arrange(date) %>%
  group_by(date) %>%
  # Select First Observation
  slice(1) %>%
  ungroup() %>%
  arrange(desc(date)) 

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:12
name_vec <- paste(prefix, suffix, sep = '')
dat_PI[, name_vec] <- NA

# Roll apply rolling averages (1 month, 2 month...n)
for(i in seq_along(name_vec)) {
  dat_PI[, 3 + i] <- rollapply(dat_PI$value, width = i, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
}

dat_PI <- dat_PI %>%
  # Normalize Columns
  mutate_at(name_vec, per_change) %>%
  mutate_at(name_vec, z_norm) %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         var = 'PI') %>%
  select(date, realtime_period, value, var, score) %>%
  drop_na()

head(dat_PI)



# Industrial Production---------------------------------------------------------
# Pull Data From Alfred
dat_INDPRO <- get_alfred_series('INDPRO', 'value') %>%
  drop_na() %>%
  arrange(date) %>%
  group_by(date) %>%
  # Select First Observation
  slice(1) %>%
  ungroup() %>%
  arrange(desc(date)) 

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:12
name_vec <- paste(prefix, suffix, sep = '')
dat_INDPRO[, name_vec] <- NA

# Roll apply rolling averages (1 month, 2 month...n)
for(i in seq_along(name_vec)) {
  dat_INDPRO[, 3 + i] <- rollapply(dat_INDPRO$value, width = i, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
}

dat_INDPRO <- dat_INDPRO %>%
  # Normalize Columns
  mutate_at(name_vec, per_change) %>%
  mutate_at(name_vec, z_norm) %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         var = 'INDPRO') %>%
  select(date, realtime_period, value, var, score) %>%
  drop_na()

head(dat_INDPRO)



# Real Personal Consumption Expenditures ---------------------------------------
# Pull Data From Alfred
dat_PCEC96 <- get_alfred_series('PCEC96', 'value') %>%
  drop_na() %>%
  arrange(date) %>%
  group_by(date) %>%
  # Select First Observation
  slice(1) %>%
  ungroup() %>%
  arrange(desc(date)) 

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:12
name_vec <- paste(prefix, suffix, sep = '')
dat_PCEC96[, name_vec] <- NA

# Roll apply rolling averages (1 month, 2 month...n)
for(i in seq_along(name_vec)) {
  dat_PCEC96[, 3 + i] <- rollapply(dat_PCEC96$value, width = i, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
}

dat_PCEC96 <- dat_PCEC96 %>%
  # Normalize Columns
  mutate_at(name_vec, per_change) %>%
  mutate_at(name_vec, z_norm) %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         var = 'PCEC96') %>%
  select(date, realtime_period, value, var, score) %>%
  drop_na()

head(dat_PCEC96)



# CPI --------------------------------------------------------------------------
# Pull Data From Alfred
dat_CPIAUCSL <- get_alfred_series('CPIAUCSL', 'value') %>%
  drop_na() %>%
  arrange(date) %>%
  group_by(date) %>%
  # Select First Observation
  slice(1) %>%
  ungroup() %>%
  arrange(desc(date)) 

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:12
name_vec <- paste(prefix, suffix, sep = '')
dat_CPIAUCSL[, name_vec] <- NA

# Roll apply rolling averages (1 month, 2 month...n)
for(i in seq_along(name_vec)) {
  dat_CPIAUCSL[, 3 + i] <- rollapply(dat_CPIAUCSL$value, width = i, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
}

dat_CPIAUCSL <- dat_CPIAUCSL %>%
  # Normalize Columns
  mutate_at(name_vec, per_change) %>%
  mutate_at(name_vec, z_norm) %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         score = score * (-1),
         var = 'CPIAUCSL') %>%
  select(date, realtime_period, value, var, score) %>%
  drop_na()

head(dat_CPIAUCSL)



# Real Disposable Income -------------------------------------------------------
# Pull Data From Alfred
dat_DSPIC96 <- get_alfred_series('DSPIC96', 'value') %>%
  drop_na() %>%
  arrange(date) %>%
  group_by(date) %>%
  # Select First Observation
  slice(1) %>%
  ungroup() %>%
  arrange(desc(date)) 

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:12
name_vec <- paste(prefix, suffix, sep = '')
dat_DSPIC96[, name_vec] <- NA

# Roll apply rolling averages (1 month, 2 month...n)
for(i in seq_along(name_vec)) {
  dat_DSPIC96[, 3 + i] <- rollapply(dat_DSPIC96$value, width = i, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
}

dat_DSPIC96 <- dat_DSPIC96 %>%
  # Normalize Columns
  mutate_at(name_vec, per_change) %>%
  mutate_at(name_vec, z_norm) %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         var = 'DSPIC96') %>%
  select(date, realtime_period, value, var, score) %>%
  drop_na()

head(dat_DSPIC96)



# S & P 500 --------------------------------------------------------------------
# Pull Data From Fred
dat_SP500 <-  get_fred_series('SP500', 'value') %>%
  drop_na() %>%
  mutate(realtime_period = date) %>%
  arrange(date) %>%
  group_by(date) %>%
  # Select First Observation
  slice(1) %>%
  ungroup() %>%
  arrange(desc(date))  %>%
  select(date, realtime_period, value)

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:252
name_vec <- paste(prefix, suffix, sep = '')
dat_SP500[, name_vec] <- NA

# Roll apply rolling averages (1 day, 2 day...n)
for(i in seq_along(name_vec)) {
  dat_SP500[, 3 + i] <- rollapply(dat_SP500$value, width = i, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
}

dat_SP500 <- dat_SP500 %>%
  # Normalize Columns
  mutate_at(name_vec, per_change) %>%
  mutate_at(name_vec, z_norm) %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         var = 'SP500') %>%
  select(date, realtime_period, value, var, score) %>%
  drop_na()

tail(dat_SP500)



# Assemble ---------------------------------------------------------------------

all_econ_vars <- dat_CPIAUCSL %>%
  #  rbind(dat_SP500) %>% get data going further back than 2011
  rbind(dat_INDPRO) %>%
  rbind(dat_PAYEMS) %>%
  rbind(dat_PCEC96) %>%
  rbind(dat_PI) %>%
  rbind(dat_DSPIC96) %>%
  drop_na()
#  mutate(weight = ifelse(var == 'GSPC', .1, .18)) weights for when SP500 is online
# ADD forecasted GDP when you can

all_var_min <- all_econ_vars %>%
  arrange(desc(date)) %>%
  group_by(date) %>%
  summarize(n = n()) %>%
  filter(n == 6) %>%
  slice_head(n = 1) %>%
  .$date

# filter to begin when all Indicators Available
econ <- all_econ_vars %>%
  filter(date >= all_var_min) %>%
  arrange(date) %>%
  select(-value)

# Date Range for Score
min_date <- min(econ$date)
max_date <- max(econ$date)
range_date <- seq(as.Date(min_date), as.Date(max_date), 'day')

# Average Score
econ_score <- map_df(range_date, function(D) {
  econ %>%
    arrange(desc(date)) %>%
    filter(date <= D) %>%
    group_by(var) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    summarize(date = D,
              realtime_period = realtime_period[1],
              var = 'SCORE',
              score = mean(score, na.rm = TRUE))
})

# Build table of score by day (for SP 500 addition)
#econ_temp <- map_df(range_date, function(D) {
#  economics %>%
#    arrange(desc(date)) %>%
#    filter(date <= D) %>%
#    group_by(var) %>%
#    slice_head(n = 1) %>%
#    ungroup() %>%
#    summarize(date = D,
#              realtime_period = realtime_period,
#              var = var,
#              score = score)
#})

# Assemble score table
econ_score <- econ_score %>%
  rbind(econ) %>%
  arrange(desc(date))

# Variable factor levels
var_levels <- c('SCORE', 'PAYEMS', 'PI', 'INDPRO', 'PCEC96', 'CPIAUCSL', 'DSPIC96')
econ_score <- econ_score %>%
  mutate(var = factor(var, levels = var_levels))

econ_score %>%
  group_by(var) %>%
  summarize(nas = any(is.na(score)))


# Smooth Scores-----------------------------------------------------------------

# CONSIDER DOING THIS?


# Save as .CSV and .RDA
write_csv(econ_score, file = paste('out/econ-score-', max(range_date), '.csv', sep = ''))
save(econ_score, file = 'rda/econ_score.rda')

# Visualize --------------------------------------------------------------------
library(RColorBrewer)

period_start <- ymd(20170120)
period_end <- ymd(20170119)

load('rda/econ_score.rda')

unique(econ_score$var)

econ_score %>%
  #filter(var %in% c('SCORE')) %>%
  ggplot(aes(x = date, y = score, color = var)) +
  geom_hline(yintercept = 0, color = 'red') +
  geom_line() +
  scale_color_brewer(palette = 'Dark2')

