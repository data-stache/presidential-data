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
  library(knitr)
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

load('rda/pres_results_prior.rda')
pres_results_prior <- pres_results_prior %>%
  filter(!state_abb %in% c('ME1', 'ME2', 'NE1', 'NE2', 'NE3', 'NAT'))

states <- unique(pres_results_prior$state_abb)



# Unemployment Rate ------------------------------------------------------------
ur_states <- paste(states, 'UR', sep = '')

# Pull Data From Alfred
dat_UR_states <- map_df(ur_states, function(X) {
  fredr(X) %>%
  arrange(desc(date))
})

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:12
name_vec <- paste(prefix, suffix, sep = '')
dat_UR_states[, name_vec] <- 0

# Roll apply rolling averages (1 month, 2 month...n)
for(i in seq_along(ur_states)) {
  for(j in seq_along(name_vec)) {
    dat_UR_states[dat_UR_states$series_id == ur_states[i], 3 + j] <- rollapply(dat_UR_states$value[dat_UR_states$series_id == ur_states[i]], width = j, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
  }
}

dat_UR_states <- dat_UR_states %>%
  group_by(series_id) %>%
  # Normalize Columns
  mutate_at(name_vec, per_change) %>%
  mutate_at(name_vec, z_norm) %>%
  ungroup() %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         score = score * (-1),
         var = 'UNRATE',
         series_id = substr(series_id, start = 1, stop = 2)) %>%
  select(date, state = series_id, value, var, score) %>%
  drop_na()

head(dat_UR_states)



# COINCIDENT ECONOMIC ACTIVITY (SET TO MATCH GDP) ------------------------------
# Pull Data From Alfred
ceai_states <- paste(states[states != 'DC'], 'PHCI', sep = '')

# Pull Data From Alfred
dat_CEAI_states <- map_df(ceai_states, function(X) {
  fredr(X) %>%
    arrange(desc(date))
})

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:12
name_vec <- paste(prefix, suffix, sep = '')
dat_CEAI_states[, name_vec] <- 0

# Roll apply rolling averages (1 month, 2 month...n)
for(i in seq_along(ceai_states)) {
  for(j in seq_along(name_vec)) {
    dat_CEAI_states[dat_CEAI_states$series_id == ceai_states[i], 3 + j] <- rollapply(dat_CEAI_states$value[dat_CEAI_states$series_id == ceai_states[i]], width = j, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
  }
}

dat_CEAI_states <- dat_CEAI_states %>%
  group_by(series_id) %>%
  # Normalize Columns
  mutate_at(name_vec, per_change) %>%
  mutate_at(name_vec, z_norm) %>%
  ungroup() %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         var = 'CEAI',
         series_id = substr(series_id, start = 1, stop = 2)) %>%
  select(date, state = series_id, value, var, score) %>%
  drop_na()

# ADD MEDIAN VALUES FOR DC SINCE OMITTED
dat_CEAI_states <- dat_CEAI_states %>%
  group_by(date) %>%
  summarize(date = date[1],
            state = 'DC',
            value = median(value, na.rm = TRUE),
            var = var[1],
            score = median(score, na.rm = TRUE)) %>%
  rbind(dat_CEAI_states)

head(dat_CEAI_states)



# New Housing ------------------------------------------------------------------
# Pull Data From Alfred
housing_states <- paste(states, 'BPPRIVSA', sep = '')

# Pull Data From Alfred
dat_HOUSING_states <- map_df(housing_states, function(X) {
  fredr(X) %>%
    arrange(desc(date))
})

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:12
name_vec <- paste(prefix, suffix, sep = '')
dat_HOUSING_states[, name_vec] <- 0

# Roll apply rolling averages (1 month, 2 month...n)
for(i in seq_along(housing_states)) {
  for(j in seq_along(name_vec)) {
    dat_HOUSING_states[dat_HOUSING_states$series_id == housing_states[i], 3 + j] <- rollapply(dat_HOUSING_states$value[dat_HOUSING_states$series_id == housing_states[i]], width = j, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
  }
}

dat_HOUSING_states <- dat_HOUSING_states %>%
  group_by(series_id) %>%
  # Normalize Columns
  mutate_at(name_vec, per_change) %>%
  mutate_at(name_vec, z_norm) %>%
  ungroup() %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         var = 'HOUSING',
         series_id = substr(series_id, start = 1, stop = 2)) %>%
  select(date, state = series_id, value, var, score) %>%
  drop_na()

head(dat_HOUSING_states)



# State Leading Indictors ------------------------------------------------------
# Pull Data From Alfred
leading_states <- paste(states[states != 'DC'], 'SLIND', sep = '')

# Pull Data From Alfred
dat_LI_states <- map_df(leading_states, function(X) {
  fredr(X) %>%
    arrange(desc(date))
})

# Set Col Names for Rolling Averages
prefix <- 'pd_'
suffix <- 1:12
name_vec <- paste(prefix, suffix, sep = '')
dat_LI_states[, name_vec] <- 0

# Roll apply rolling averages (1 month, 2 month...n)
for(i in seq_along(leading_states)) {
  for(j in seq_along(name_vec)) {
    dat_LI_states[dat_LI_states$series_id == leading_states[i], 3 + j] <- rollapply(dat_LI_states$value[dat_LI_states$series_id == leading_states[i]], width = j, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
  }
}

dat_LI_states <- dat_LI_states %>%
  group_by(series_id) %>%
  # Normalize Columns 
  # NO PERCENT CHANGE HERE!
  mutate_at(name_vec, z_norm) %>%
  ungroup() %>%
  # Average across observations
  mutate(score = rowMeans(select(., pd_1:pd_12)),
         var = 'LEADING',
         series_id = substr(series_id, start = 1, stop = 2)) %>%
  select(date, state = series_id, value, var, score) %>%
  drop_na()

# ADD MEDIAN VALUES FOR DC SINCE OMITTED
dat_LI_states <- dat_LI_states %>%
  group_by(date) %>%
  summarize(date = date[1],
            state = 'DC',
            value = median(value, na.rm = TRUE),
            var = var[1],
            score = median(score, na.rm = TRUE)) %>%
  rbind(dat_LI_states)

head(dat_LI_states)



# Assemble ---------------------------------------------------------------------

all_econ_vars <- dat_CEAI_states %>%
  rbind(dat_HOUSING_states) %>%
  rbind(dat_LI_states) %>%
  rbind(dat_UR_states) %>%
  drop_na()

min(dat_CEAI_states$date)
min(dat_HOUSING_states$date)
min(dat_LI_states$date)
min(dat_UR_states$date)

# filter to begin when all Indicators Available
econ <- all_econ_vars %>%
  arrange(date) %>%
  select(-value)

# Date Range for Score
min_date <- min(econ$date)
max_date <- max(econ$date)
range_date <- seq(as.Date(min_date), as.Date(max_date), 'day')

SAEIs <- econ %>%
  group_by(date, state) %>%
  summarize(var = 'SAEI', # State Average Economic Indicator
            score = mean(score))

econ <- econ %>%
  rbind(SAEIs) %>%
  arrange(date, state)

unique(econ$var)

# Variable factor levels
var_levels <- c('SAEI', 'UNRATE', 'CEAI', 'LEADING', 'HOUSING')
econ <- econ %>%
  mutate(var = factor(var, levels = var_levels))

econ %>%
  group_by(var) %>%
  summarize(nas = any(is.na(score)))



# Save as .CSV and .RDA
write_csv(econ, file = paste('out/econ-score-state', max(range_date), '.csv', sep = ''))
save(econ, file = 'rda/state_econ_score.rda')

