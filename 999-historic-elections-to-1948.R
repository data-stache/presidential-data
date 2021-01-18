# 2020 ELECTION RESULTS

library(lubridate)
library(stringr)
library(tidyverse)
library(rvest)
library(tidylog)

RUN_DATE <- Sys.Date()
CURRENT_CYCLE <- 2024

# Graph Colors
PARTY_COL <- c('darkblue', 'red', 'green4')
PARTY_LEV <- c('dem', 'rep', 'other')

potus_results_76_20_tidy <- read.csv("data/potus_results_76_20_tidy.csv", stringsAsFactors = FALSE, header = TRUE) %>%
  select(-X) %>%
  arrange(cycle, state) %>%
  # Make Party a factor for aesthetic mappings / easier maths
  mutate(election_day = ymd(election_day),
         run_date = ymd(run_date))

head(potus_results_76_20_tidy)

dat_NAT <- potus_results_76_20_tidy %>%
  group_by(cycle, party) %>%
  summarize(state = 'National',
            state_abb = 'NAT',
            total_votes = sum(total_votes, na.rm = TRUE),
            election_day = election_day[1],
            incumbant_party = incumbant_party[1],
            winning_party = winning_party[1],
            state_winner = state_winner[1],
            run_date = run_date[1]) %>%
  ungroup() %>%
  group_by(cycle) %>%
  mutate(vote_share = total_votes / sum(total_votes)) %>%
  select(cycle, state, state_abb, party, vote_share, total_votes, election_day, incumbant_party, winning_party, state_winner, run_date)

potus_results_76_20_tidy <- potus_results_76_20_tidy %>%
  rbind(dat_NAT) %>%
  arrange(cycle)


# 1972 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1972_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1972 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1972 <- pres_results_1972[[15]] %>% html_table(fill = TRUE)

head(pres_results_1972)

# RENAME COLUMNS
pres_results_1972 <- pres_results_1972 %>% setNames(c('state', 'ev',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'american_independent_total', 'american_independent_share', 'american_independent_votes', 
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes', 
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1972 <- pres_results_1972[c(-1, -53), c(-2, -5, -8, -11, -14:-16)]
pres_results_1972$state[pres_results_1972$state == 'D.C.'] <- 'District of Columbia'

pres_results_1972 <- pres_results_1972 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'american_independent_total', 'libertarian_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'american_independent_total', 'libertarian_total', 'total_votes'),
            .funs = gsub,
            pattern = '-',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         rep_total = ifelse(is.na(rep_total), 0, rep_total),
         dem_total = as.numeric(dem_total),
         dem_total = ifelse(is.na(dem_total), 0, dem_total),
         american_independent_total = as.numeric(american_independent_total),
         american_independent_total = ifelse(is.na(american_independent_total), 0, american_independent_total),
         libertarian_total = as.numeric(libertarian_total),
         libertarian_total = ifelse(is.na(libertarian_total), 0, libertarian_total),
         total_votes = as.numeric(total_votes),
         total_votes = ifelse(is.na(total_votes), 0, total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         american_independent_share = american_independent_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         other_total = total_votes - (rep_total + dem_total + american_independent_total + libertarian_total),
         other_share = other_total / total_votes)

head(pres_results_1972)

pres_res_1972 <- pres_results_1972 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, american_independent = american_independent_total, libertarian = libertarian_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1972 <- pres_results_1972 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, american_independent = american_independent_share, libertarian = libertarian_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1972) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'american_independent', 'libertarian', 'other')),
         election_day = ymd(19721107),
         incumbant_party = 'rep',
         winning_party = 'rep') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1972)

pres_results_1972 <- pres_results_1972[, c(11, 1:10)]

dat_NAT <- pres_results_1972 %>%
  group_by(cycle, party) %>%
  summarize(state = 'National',
            state_abb = 'NAT',
            total_votes = sum(total_votes, na.rm = TRUE),
            election_day = election_day[1],
            incumbant_party = incumbant_party[1],
            winning_party = winning_party[1],
            state_winner = state_winner[1],
            run_date = run_date[1]) %>%
  ungroup() %>%
  group_by(cycle) %>%
  arrange(party) %>%
  mutate(vote_share = total_votes / sum(total_votes),
         state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep')) %>%
  select(cycle, state, state_abb, party, vote_share, total_votes, election_day, incumbant_party, winning_party, state_winner, run_date)

pres_results_1972 <- pres_results_1972 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_76_20_tidy <- potus_results_76_20_tidy %>%
  rbind(pres_results_1972) %>%
  arrange(cycle)



# 1968 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1968_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1968 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1968 <- pres_results_1968[[21]] %>% html_table(fill = TRUE)

head(pres_results_1968)

# RENAME COLUMNS
pres_results_1968 <- pres_results_1968 %>% setNames(c('state', 'ev',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'american_independent_total', 'american_independent_share', 'american_independent_votes', 
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1968 <- pres_results_1968[c(-1, -53), c(-2, -5, -8, -11:-13)]
pres_results_1968$state[pres_results_1968$state == 'D.C.'] <- 'District of Columbia'

pres_results_1968 <- pres_results_1968 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'american_independent_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'american_independent_total', 'total_votes'),
            .funs = gsub,
            pattern = '-',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         american_independent_total = as.numeric(american_independent_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         american_independent_share = american_independent_total / total_votes,
         other_total = total_votes - (rep_total + dem_total + american_independent_total),
         other_share = other_total / total_votes)

head(pres_results_1968)

pres_res_1968 <- pres_results_1968 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, american_independent = american_independent_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1968 <- pres_results_1968 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, american_independent = american_independent_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1968) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'american_independent', 'other')),
         election_day = ymd(19681105),
         incumbant_party = 'dem',
         winning_party = 'rep') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[3] > vote_share[2] & vote_share[3] > vote_share[1], 'american_independent', ifelse(vote_share[1] > vote_share[2], 'dem', 'rep')),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1968)

pres_results_1968 <- pres_results_1968[, c(11, 1:10)]

dat_NAT <- pres_results_1968 %>%
  group_by(cycle, party) %>%
  summarize(state = 'National',
            state_abb = 'NAT',
            total_votes = sum(total_votes, na.rm = TRUE),
            election_day = election_day[1],
            incumbant_party = incumbant_party[1],
            winning_party = winning_party[1],
            state_winner = state_winner[1],
            run_date = run_date[1]) %>%
  ungroup() %>%
  group_by(cycle) %>%
  arrange(party) %>%
  mutate(vote_share = total_votes / sum(total_votes),
         state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep')) %>%
  select(cycle, state, state_abb, party, vote_share, total_votes, election_day, incumbant_party, winning_party, state_winner, run_date)

pres_results_1968 <- pres_results_1968 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_76_20_tidy <- potus_results_76_20_tidy %>%
  rbind(pres_results_1968) %>%
  arrange(cycle)



# 1964 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1964_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1964 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1964 <- pres_results_1964[[14]] %>% html_table(fill = TRUE)

head(pres_results_1964)

# RENAME COLUMNS
pres_results_1964 <- pres_results_1964 %>% setNames(c('state', 'ev',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'unpledged_dem_total', 'unpledged_dem_share', 'unpledged_dem_votes', 
                                                      'socialist_labor_total', 'socialist_labor_share', 'socialist_labor_votes', 
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1964 <- pres_results_1964[c(-1, -53), c(-2, -5, -8, -11, -14:-16)]
pres_results_1964$state[pres_results_1964$state == 'D.C.'] <- 'District of Columbia'

pres_results_1964 <- pres_results_1964 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'unpledged_dem_total', 'socialist_labor_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'unpledged_dem_total', 'socialist_labor_total', 'total_votes'),
            .funs = gsub,
            pattern = '-',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         unpledged_dem_total = as.numeric(unpledged_dem_total),
         socialist_labor_total = as.numeric(socialist_labor_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         unpledged_dem_share = unpledged_dem_total / total_votes,
         socialist_labor_share = socialist_labor_total / total_votes,
         other_total = total_votes - (rep_total + dem_total + socialist_labor_total),
         other_share = other_total / total_votes)

head(pres_results_1964)

pres_res_1964 <- pres_results_1964 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, unpledged_dem = unpledged_dem_total, socialist_labor = socialist_labor_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1964 <- pres_results_1964 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, unpledged_dem = unpledged_dem_share, socialist_labor = socialist_labor_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1964) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'unpledged_dem', 'socialist_labor', 'other')),
         election_day = ymd(19641103),
         incumbant_party = 'dem',
         winning_party = 'dem') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1964)

pres_results_1964 <- pres_results_1964[, c(11, 1:10)]

dat_NAT <- pres_results_1964 %>%
  group_by(cycle, party) %>%
  summarize(state = 'National',
            state_abb = 'NAT',
            total_votes = sum(total_votes, na.rm = TRUE),
            election_day = election_day[1],
            incumbant_party = incumbant_party[1],
            winning_party = winning_party[1],
            state_winner = state_winner[1],
            run_date = run_date[1]) %>%
  ungroup() %>%
  group_by(cycle) %>%
  arrange(party) %>%
  mutate(vote_share = total_votes / sum(total_votes),
         state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep')) %>%
  select(cycle, state, state_abb, party, vote_share, total_votes, election_day, incumbant_party, winning_party, state_winner, run_date)

pres_results_1964 <- pres_results_1964 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_76_20_tidy <- potus_results_76_20_tidy %>%
  rbind(pres_results_1964) %>%
  arrange(cycle)



# 1960 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1960_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1960 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1960 <- pres_results_1960[[14]] %>% html_table(fill = TRUE)

head(pres_results_1960)

# RENAME COLUMNS
pres_results_1960 <- pres_results_1960 %>% setNames(c('state', 'ev',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'unpledged_dem_total', 'unpledged_dem_share', 'unpledged_dem_votes', 
                                                      'socialist_labor_total', 'socialist_labor_share', 'socialist_labor_votes', 
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1960 <- pres_results_1960[c(-1, -52), c(-2, -5, -8, -11, -14:-16)]

pres_results_1960 <- pres_results_1960 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'unpledged_dem_total', 'socialist_labor_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'unpledged_dem_total', 'socialist_labor_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         unpledged_dem_total = as.numeric(unpledged_dem_total),
         socialist_labor_total = as.numeric(socialist_labor_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         unpledged_dem_share = unpledged_dem_total / total_votes,
         socialist_labor_share = socialist_labor_total / total_votes,
         other_total = total_votes - (rep_total + dem_total + socialist_labor_total),
         other_share = other_total / total_votes)

head(pres_results_1960)

pres_res_1960 <- pres_results_1960 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, unpledged_dem = unpledged_dem_total, socialist_labor = socialist_labor_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1960 <- pres_results_1960 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, unpledged_dem = unpledged_dem_share, socialist_labor = socialist_labor_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1960) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'unpledged_dem', 'socialist_labor', 'other')),
         election_day = ymd(19601108),
         incumbant_party = 'rep',
         winning_party = 'dem') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1960)

pres_results_1960 <- pres_results_1960[, c(11, 1:10)]

dat_NAT <- pres_results_1960 %>%
  group_by(cycle, party) %>%
  summarize(state = 'National',
            state_abb = 'NAT',
            total_votes = sum(total_votes, na.rm = TRUE),
            election_day = election_day[1],
            incumbant_party = incumbant_party[1],
            winning_party = winning_party[1],
            state_winner = state_winner[1],
            run_date = run_date[1]) %>%
  ungroup() %>%
  group_by(cycle) %>%
  arrange(party) %>%
  mutate(vote_share = total_votes / sum(total_votes),
         state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep')) %>%
  select(cycle, state, state_abb, party, vote_share, total_votes, election_day, incumbant_party, winning_party, state_winner, run_date)

pres_results_1960 <- pres_results_1960 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_76_20_tidy <- potus_results_76_20_tidy %>%
  rbind(pres_results_1960) %>%
  arrange(cycle)



# 1956 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1956_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1956 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1956 <- pres_results_1956[[18]] %>% html_table(fill = TRUE)

head(pres_results_1956)

# RENAME COLUMNS
pres_results_1956 <- pres_results_1956 %>% setNames(c('state', 'ev',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'states_rights_total', 'states_rights_share', 'states_rights_votes', 
                                                      'socialist_labor_total', 'socialist_labor_share', 'socialist_labor_votes', 
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1956 <- pres_results_1956[c(-1, -50), c(-2, -5, -8, -11, -14:-16)]

pres_results_1956 <- pres_results_1956 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'states_rights_total', 'socialist_labor_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'states_rights_total', 'socialist_labor_total', 'total_votes'),
            .funs = gsub,
            pattern = '-',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         states_rights_total = as.numeric(states_rights_total),
         socialist_labor_total = as.numeric(socialist_labor_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         states_rights_share = states_rights_total / total_votes,
         socialist_labor_share = socialist_labor_total / total_votes,
         other_total = total_votes - (rep_total + dem_total + states_rights_total + socialist_labor_total),
         other_total = ifelse(other_total < 0, 0, other_total),
         other_share = other_total / total_votes)

head(pres_results_1956)

pres_res_1956 <- pres_results_1956 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, states_rights = states_rights_total, socialist_labor = socialist_labor_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1956 <- pres_results_1956 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, states_rights = states_rights_share, socialist_labor = socialist_labor_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1956) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'states_rights', 'socialist_labor', 'other')),
         election_day = ymd(19561106),
         incumbant_party = 'rep',
         winning_party = 'rep') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1956)

pres_results_1956 <- pres_results_1956[, c(11, 1:10)]

dat_NAT <- pres_results_1956 %>%
  group_by(cycle, party) %>%
  summarize(state = 'National',
            state_abb = 'NAT',
            total_votes = sum(total_votes, na.rm = TRUE),
            election_day = election_day[1],
            incumbant_party = incumbant_party[1],
            winning_party = winning_party[1],
            state_winner = state_winner[1],
            run_date = run_date[1]) %>%
  ungroup() %>%
  group_by(cycle) %>%
  arrange(party) %>%
  mutate(vote_share = total_votes / sum(total_votes),
         state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep')) %>%
  select(cycle, state, state_abb, party, vote_share, total_votes, election_day, incumbant_party, winning_party, state_winner, run_date)

pres_results_1956 <- pres_results_1956 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_76_20_tidy <- potus_results_76_20_tidy %>%
  rbind(pres_results_1956) %>%
  arrange(cycle)



# 1952 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1952_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1952 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1952 <- pres_results_1952[[14]] %>% html_table(fill = TRUE)

head(pres_results_1952)

# RENAME COLUMNS
pres_results_1952 <- pres_results_1952 %>% setNames(c('state', 'ev',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'progressive_total', 'progressive_share', 'progressive_votes', 
                                                      'prohibition_total', 'prohibition_share', 'prohibition_votes', 
                                                      'socialist_labor_total', 'socialist_labor_share', 'socialist_labor_votes', 
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1952 <- pres_results_1952[c(-1, -50), c(-2, -5, -8, -11, -14, -17:-19)]

pres_results_1952 <- pres_results_1952 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'progressive_total', 'prohibition_total', 'socialist_labor_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'progressive_total', 'prohibition_total', 'socialist_labor_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         progressive_total = as.numeric(progressive_total),
         prohibition_total = as.numeric(prohibition_total),
         socialist_labor_total = as.numeric(socialist_labor_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         progressive_share = progressive_total / total_votes,
         prohibition_share = prohibition_total / total_votes,
         socialist_labor_share = socialist_labor_total / total_votes,
         other_total = total_votes - (rep_total + dem_total + progressive_total + prohibition_total + socialist_labor_total),
         other_share = other_total / total_votes)

head(pres_results_1952)

pres_res_1952 <- pres_results_1952 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, progressive = progressive_total, prohibition = prohibition_total, socialist_labor = socialist_labor_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1952 <- pres_results_1952 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, progressive = progressive_share, prohibition = prohibition_share, socialist_labor = socialist_labor_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1952) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'progressive', 'prohibition', 'socialist_labor', 'other')),
         election_day = ymd(19521104),
         incumbant_party = 'dem',
         winning_party = 'rep') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1952)

pres_results_1952 <- pres_results_1952[, c(11, 1:10)]

dat_NAT <- pres_results_1952 %>%
  group_by(cycle, party) %>%
  summarize(state = 'National',
            state_abb = 'NAT',
            total_votes = sum(total_votes, na.rm = TRUE),
            election_day = election_day[1],
            incumbant_party = incumbant_party[1],
            winning_party = winning_party[1],
            state_winner = state_winner[1],
            run_date = run_date[1]) %>%
  ungroup() %>%
  group_by(cycle) %>%
  arrange(party) %>%
  mutate(vote_share = total_votes / sum(total_votes),
         state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep')) %>%
  select(cycle, state, state_abb, party, vote_share, total_votes, election_day, incumbant_party, winning_party, state_winner, run_date)

pres_results_1952 <- pres_results_1952 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_76_20_tidy <- potus_results_76_20_tidy %>%
  rbind(pres_results_1952) %>%
  arrange(cycle)



# 1948 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1948_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1948 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1948 <- pres_results_1948[[20]] %>% html_table(fill = TRUE)

head(pres_results_1948)

# RENAME COLUMNS
pres_results_1948 <- pres_results_1948 %>% setNames(c('state', 'ev',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'dixiecrat_total', 'dixiecrat_share', 'dixiecrat_votes', 
                                                      'progressive_total', 'progressive_share', 'progressive_votes', 
                                                      'socialist_total', 'socialist_share', 'socialist_votes', 
                                                      'other_total', 'other_share', 'other_votes', 
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1948 <- pres_results_1948[c(-1, -50), c(-2, -5, -8, -11, -14, -17, -20:-22)]

pres_results_1948 <- pres_results_1948 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'dixiecrat_total', 'progressive_total', 'socialist_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'dixiecrat_total', 'progressive_total', 'socialist_total', 'total_votes'),
            .funs = gsub,
            pattern = '-',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         dixiecrat_total = as.numeric(dixiecrat_total),
         progressive_total = as.numeric(progressive_total),
         socialist_total = as.numeric(socialist_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         dixiecrat_share = dixiecrat_total / total_votes,
         progressive_share = progressive_total / total_votes,
         socialist_share = socialist_total / total_votes,
         other_total = total_votes - (rep_total + dem_total + dixiecrat_total + progressive_total + socialist_total),
         other_share = other_total / total_votes)

head(pres_results_1948)

pres_res_1948 <- pres_results_1948 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, dixiecrat = dixiecrat_total, progressive = progressive_total, socialist = socialist_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1948 <- pres_results_1948 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, dixiecrat = dixiecrat_share, progressive = progressive_share, socialist = socialist_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1948) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'dixiecrat', 'progressive', 'socialist', 'other')),
         election_day = ymd(19481107),
         incumbant_party = 'dem',
         winning_party = 'dem') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[3] > vote_share[1] & vote_share[3] > vote_share[2], 'dixiecrat', ifelse(vote_share[1] > vote_share[2], 'dem', 'rep')),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1948)

pres_results_1948 <- pres_results_1948[, c(11, 1:10)]

dat_NAT <- pres_results_1948 %>%
  group_by(cycle, party) %>%
  summarize(state = 'National',
            state_abb = 'NAT',
            total_votes = sum(total_votes, na.rm = TRUE),
            election_day = election_day[1],
            incumbant_party = incumbant_party[1],
            winning_party = winning_party[1],
            state_winner = state_winner[1],
            run_date = run_date[1]) %>%
  ungroup() %>%
  group_by(cycle) %>%
  arrange(party) %>%
  mutate(vote_share = total_votes / sum(total_votes),
         state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep')) %>%
  select(cycle, state, state_abb, party, vote_share, total_votes, election_day, incumbant_party, winning_party, state_winner, run_date)

pres_results_1948 <- pres_results_1948 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_76_20_tidy <- potus_results_76_20_tidy %>%
  rbind(pres_results_1948) %>%
  arrange(cycle) %>%
  mutate(run_date = RUN_DATE)

write.csv(potus_results_76_20_tidy, 'data/potus_results_76_20_tidy.csv')







