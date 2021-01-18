# 2020 ELECTION RESULTS

library(lubridate)
library(stringr)
library(tidyverse)
library(rvest)
library(tidylog)

RUN_DATE <- Sys.Date()


load('rda/state_abbs.rda')


# 2020 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/2020_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_2020 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_2020 <- pres_results_2020[grep("State or", pres_results_2020, ignore.case = T)]
pres_results_2020 <- html_table(pres_results_2020[grep("state or", pres_results_2020, ignore.case = T)], fill = T)[[1]]

head(pres_results_2020)

# RENAME COLUMNS
pres_results_2020 <- pres_results_2020 %>% setNames(c('state',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes',
                                                      'green_total', 'green_share', 'green_votes',
                                                      'other_total', 'other_share', 'other_votes',
                                                      'matgin_votes', 'margin_share', 'margin_swing',
                                                      'total_votes', 'sources'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_2020 <- pres_results_2020[c(-1, -58:-59), c(-4, -7, -10, -13, -16:-19, -21)]
{
  pres_results_2020$state[pres_results_2020$state == 'Ala.'] <- 'Alabama'
  pres_results_2020$state[pres_results_2020$state == 'Ariz.'] <- 'Arizona'
  pres_results_2020$state[pres_results_2020$state == 'Ark.'] <- 'Arkansas'
  pres_results_2020$state[pres_results_2020$state == 'Calif.'] <- 'California'
  pres_results_2020$state[pres_results_2020$state == 'Colo.'] <- 'Colorado'
  pres_results_2020$state[pres_results_2020$state == 'Conn.'] <- 'Connecticut'
  pres_results_2020$state[pres_results_2020$state == 'Del.'] <- 'Delaware'
  pres_results_2020$state[pres_results_2020$state == 'D.C.'] <- 'District of Columbia'
  pres_results_2020$state[pres_results_2020$state == 'Fla.'] <- 'Florida'
  pres_results_2020$state[pres_results_2020$state == 'Ga.'] <- 'Georgia'
  pres_results_2020$state[pres_results_2020$state == 'Ill.'] <- 'Illinois'
  pres_results_2020$state[pres_results_2020$state == 'Ind.'] <- 'Indiana'
  pres_results_2020$state[pres_results_2020$state == 'Kan.'] <- 'Kansas'
  pres_results_2020$state[pres_results_2020$state == 'Ky.'] <- 'Kentucky'
  pres_results_2020$state[pres_results_2020$state == 'La.'] <- 'Louisiana'
  pres_results_2020$state[pres_results_2020$state == 'Maine †'] <- 'Maine'
  pres_results_2020$state[pres_results_2020$state == 'ME-1'] <- 'Maine CD-1'
  pres_results_2020$state[pres_results_2020$state == 'ME-2'] <- 'Maine CD-2'
  pres_results_2020$state[pres_results_2020$state == 'Md.'] <- 'Maryland'
  pres_results_2020$state[pres_results_2020$state == 'Mass.'] <- 'Massachusetts'
  pres_results_2020$state[pres_results_2020$state == 'Mich.'] <- 'Michigan'
  pres_results_2020$state[pres_results_2020$state == 'Minn.'] <- 'Minnesota'
  pres_results_2020$state[pres_results_2020$state == 'Miss.'] <- 'Mississippi'
  pres_results_2020$state[pres_results_2020$state == 'Mo.'] <- 'Missouri'
  pres_results_2020$state[pres_results_2020$state == 'Mont.'] <- 'Montana'
  pres_results_2020$state[pres_results_2020$state == 'Neb. †'] <- 'Nebraska'
  pres_results_2020$state[pres_results_2020$state == 'NE-1'] <- 'Nebraska CD-1'
  pres_results_2020$state[pres_results_2020$state == 'NE-2'] <- 'Nebraska CD-2'
  pres_results_2020$state[pres_results_2020$state == 'NE-3'] <- 'Nebraska CD-3'
  pres_results_2020$state[pres_results_2020$state == 'Nev.'] <- 'Nevada'
  pres_results_2020$state[pres_results_2020$state == 'N.H.'] <- 'New Hampshire'
  pres_results_2020$state[pres_results_2020$state == 'N.J.'] <- 'New Jersey'
  pres_results_2020$state[pres_results_2020$state == 'N.M.'] <- 'New Mexico'
  pres_results_2020$state[pres_results_2020$state == 'N.Y.'] <- 'New York'
  pres_results_2020$state[pres_results_2020$state == 'N.C.'] <- 'North Carolina'
  pres_results_2020$state[pres_results_2020$state == 'N.D.'] <- 'North Dakota'
  pres_results_2020$state[pres_results_2020$state == 'Okla.'] <- 'Oklahoma'
  pres_results_2020$state[pres_results_2020$state == 'Ore.'] <- 'Oregon'
  pres_results_2020$state[pres_results_2020$state == 'Pa.'] <- 'Pennsylvania'
  pres_results_2020$state[pres_results_2020$state == 'R.I.'] <- 'Rhode Island'
  pres_results_2020$state[pres_results_2020$state == 'S.C.'] <- 'South Carolina'
  pres_results_2020$state[pres_results_2020$state == 'S.D.'] <- 'South Dakota'
  pres_results_2020$state[pres_results_2020$state == 'Tenn.'] <- 'Tennessee'
  pres_results_2020$state[pres_results_2020$state == 'Okla.'] <- 'Oklahoma'
  pres_results_2020$state[pres_results_2020$state == 'Vt.'] <- 'Vermont'
  pres_results_2020$state[pres_results_2020$state == 'Va.'] <- 'Virginia'
  pres_results_2020$state[pres_results_2020$state == 'Wash.'] <- 'Washington'
  pres_results_2020$state[pres_results_2020$state == 'W.Va.'] <- 'West Virginia'
  pres_results_2020$state[pres_results_2020$state == 'Wis.'] <- 'Wisconsin'
  pres_results_2020$state[pres_results_2020$state == 'Wyo.'] <- 'Wyoming'
}

pres_results_2020 <- pres_results_2020 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'green_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'green_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'green_total', 'other_total', 'total_votes'),
            .funs = str_replace,
            pattern = '\\[[a-z]\\]',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         libertarian_total = as.numeric(libertarian_total),
         green_total = as.numeric(green_total),
         other_total = as.numeric(other_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         green_share = green_total / total_votes,
         other_share = other_total / total_votes) %>%
  left_join(state_abbs)

head(pres_results_2020)

pres_res_2020 <- pres_results_2020 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, libertarian = libertarian_total, green = green_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_2020 <- pres_results_2020 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, libertarian = libertarian_share, green = green_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_2020) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'libertarian', 'green', 'other')),
         election_day = ymd(20201103),
         incumbant_party = 'rep',
         winning_party = 'dem') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 2020)

pres_results_2020 <- pres_results_2020[, c(11, 1:10)]

dat_NAT <- pres_results_2020 %>%
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

pres_results_2020 <- pres_results_2020 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- pres_results_2020 %>%
  rbind(pres_results_2020) %>%
  arrange(cycle)



# 2016 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/2016_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_2016 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_2016 <- pres_results_2016[grep("State or", pres_results_2016, ignore.case = T)]
pres_results_2016 <- html_table(pres_results_2016[grep("state or", pres_results_2016, ignore.case = T)], fill = T)[[1]]

head(pres_results_2016)

# RENAME COLUMNS
pres_results_2016 <- pres_results_2016 %>% setNames(c('state',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes',
                                                      'green_total', 'green_share', 'green_votes',
                                                      'independent_total', 'independent_share', 'independent_votes',
                                                      'other_total', 'other_share', 'other_votes',
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_2016 <- pres_results_2016[c(-1, -58:-59), c(-4, -7, -10, -13, -16, -19, -20:-21, -23)]
{
pres_results_2016$state[pres_results_2016$state == 'Ala.'] <- 'Alabama'
pres_results_2016$state[pres_results_2016$state == 'Ariz.'] <- 'Arizona'
pres_results_2016$state[pres_results_2016$state == 'Ark.'] <- 'Arkansas'
pres_results_2016$state[pres_results_2016$state == 'Calif.'] <- 'California'
pres_results_2016$state[pres_results_2016$state == 'Colo.'] <- 'Colorado'
pres_results_2016$state[pres_results_2016$state == 'Conn.'] <- 'Connecticut'
pres_results_2016$state[pres_results_2016$state == 'Del.'] <- 'Delaware'
pres_results_2016$state[pres_results_2016$state == 'D.C.'] <- 'District of Columbia'
pres_results_2016$state[pres_results_2016$state == 'Fla.'] <- 'Florida'
pres_results_2016$state[pres_results_2016$state == 'Ga.'] <- 'Georgia'
pres_results_2016$state[pres_results_2016$state == 'Ill.'] <- 'Illinois'
pres_results_2016$state[pres_results_2016$state == 'Ind.'] <- 'Indiana'
pres_results_2016$state[pres_results_2016$state == 'Kan.'] <- 'Kansas'
pres_results_2016$state[pres_results_2016$state == 'Ky.'] <- 'Kentucky'
pres_results_2016$state[pres_results_2016$state == 'La.'] <- 'Louisiana'
pres_results_2016$state[pres_results_2016$state == 'Maine †'] <- 'Maine'
pres_results_2016$state[pres_results_2016$state == 'ME-1'] <- 'Maine CD-1'
pres_results_2016$state[pres_results_2016$state == 'ME-2'] <- 'Maine CD-2'
pres_results_2016$state[pres_results_2016$state == 'Md.'] <- 'Maryland'
pres_results_2016$state[pres_results_2016$state == 'Mass.'] <- 'Massachusetts'
pres_results_2016$state[pres_results_2016$state == 'Mich.'] <- 'Michigan'
pres_results_2016$state[pres_results_2016$state == 'Minn.'] <- 'Minnesota'
pres_results_2016$state[pres_results_2016$state == 'Miss.'] <- 'Mississippi'
pres_results_2016$state[pres_results_2016$state == 'Mo.'] <- 'Missouri'
pres_results_2016$state[pres_results_2016$state == 'Mont.'] <- 'Montana'
pres_results_2016$state[pres_results_2016$state == 'Nebr. †'] <- 'Nebraska'
pres_results_2016$state[pres_results_2016$state == 'NE-1'] <- 'Nebraska CD-1'
pres_results_2016$state[pres_results_2016$state == 'NE-2'] <- 'Nebraska CD-2'
pres_results_2016$state[pres_results_2016$state == 'NE-3'] <- 'Nebraska CD-3'
pres_results_2016$state[pres_results_2016$state == 'Nev.'] <- 'Nevada'
pres_results_2016$state[pres_results_2016$state == 'N.H.'] <- 'New Hampshire'
pres_results_2016$state[pres_results_2016$state == 'N.J.'] <- 'New Jersey'
pres_results_2016$state[pres_results_2016$state == 'N.M.'] <- 'New Mexico'
pres_results_2016$state[pres_results_2016$state == 'N.Y.'] <- 'New York'
pres_results_2016$state[pres_results_2016$state == 'N.C.'] <- 'North Carolina'
pres_results_2016$state[pres_results_2016$state == 'N.D.'] <- 'North Dakota'
pres_results_2016$state[pres_results_2016$state == 'Okla.'] <- 'Oklahoma'
pres_results_2016$state[pres_results_2016$state == 'Ore.'] <- 'Oregon'
pres_results_2016$state[pres_results_2016$state == 'Pa.'] <- 'Pennsylvania'
pres_results_2016$state[pres_results_2016$state == 'R.I.'] <- 'Rhode Island'
pres_results_2016$state[pres_results_2016$state == 'S.C.'] <- 'South Carolina'
pres_results_2016$state[pres_results_2016$state == 'S.D.'] <- 'South Dakota'
pres_results_2016$state[pres_results_2016$state == 'Tenn.'] <- 'Tennessee'
pres_results_2016$state[pres_results_2016$state == 'Okla.'] <- 'Oklahoma'
pres_results_2016$state[pres_results_2016$state == 'Vt.'] <- 'Vermont'
pres_results_2016$state[pres_results_2016$state == 'Va.'] <- 'Virginia'
pres_results_2016$state[pres_results_2016$state == 'Wash.'] <- 'Washington'
pres_results_2016$state[pres_results_2016$state == 'W.Va.'] <- 'West Virginia'
pres_results_2016$state[pres_results_2016$state == 'Wis.'] <- 'Wisconsin'
pres_results_2016$state[pres_results_2016$state == 'Wyo.'] <- 'Wyoming'
}

pres_results_2016 <- pres_results_2016 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'green_total', 'independent_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'green_total',  'independent_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         libertarian_total = as.numeric(libertarian_total),
         green_total = as.numeric(green_total),
         independent_total = as.numeric(independent_total),
         other_total = as.numeric(other_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         green_share = green_total / total_votes,
         independent_share = independent_total / total_votes,
         other_share = other_total / total_votes) %>%
  left_join(state_abbs)

head(pres_results_2016)

pres_res_2016 <- pres_results_2016 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, libertarian = libertarian_total, green = green_total, independent = independent_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_2016 <- pres_results_2016 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, libertarian = libertarian_share, green = green_share, independent = independent_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_2016) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'libertarian', 'green', 'independent', 'other')),
         election_day = ymd(20161108),
         incumbant_party = 'dem',
         winning_party = 'rep') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 2016)

pres_results_2016 <- pres_results_2016[, c(11, 1:10)]

dat_NAT <- pres_results_2016 %>%
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

pres_results_2016 <- pres_results_2016 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_2016) %>%
  arrange(cycle)



# 2012 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/2012_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_2012 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_2012 <- pres_results_2012[grep("state/district", pres_results_2012, ignore.case = T)]
pres_results_2012 <- html_table(pres_results_2012[grep("state/district", pres_results_2012, ignore.case = T)], fill = T)[[1]]

head(pres_results_2012)

# RENAME COLUMNS
pres_results_2012 <- pres_results_2012 %>% setNames(c('state',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes',
                                                      'green_total', 'green_share', 'green_votes',
                                                      'other_total', 'other_share', 'other_votes',
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_2012 <- pres_results_2012[c(-1:-2, -54), c(-4, -7, -10, -13, -16:-18)]
pres_results_2012$state[pres_results_2012$state == 'District of ColumbiaDistrict of Columbia'] <- 'District of Columbia'
pres_results_2012$state[pres_results_2012$state == 'Maine★'] <- 'Maine'
pres_results_2012$state[pres_results_2012$state == "New Jersey[116]"] <- 'New Jersey'
pres_results_2012$state[pres_results_2012$state == "New York[117]"] <- 'New York'
pres_results_2012$state[pres_results_2012$state == 'Nebraska★'] <- 'Nebraska'
pres_results_2012$state[pres_results_2012$state == "Ohio[118]"] <- 'Ohio'
pres_results_2012$state[pres_results_2012$state == "Wisconsin[119]"] <- 'Wisconsin'

pres_results_2012 <- pres_results_2012 %>%
  select(-state_abb) %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'green_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'green_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         libertarian_total = as.numeric(libertarian_total),
         green_total = as.numeric(green_total),
         other_total = as.numeric(other_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         green_share = green_total / total_votes,
         other_share = other_total / total_votes) %>%
  left_join(state_abbs)

head(pres_results_2012)

pres_res_2012 <- pres_results_2012 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, libertarian = libertarian_total, green = green_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_2012 <- pres_results_2012 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, libertarian = libertarian_share, green = green_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_2012) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'libertarian', 'green', 'other')),
         election_day = ymd(20121106),
         incumbant_party = 'dem',
         winning_party = 'dem') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 2012)

pres_results_2012 <- pres_results_2012[, c(11, 1:10)]

dat_NAT <- pres_results_2012 %>%
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

pres_results_2012 <- pres_results_2012 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_2012) %>%
  arrange(cycle)



# 2008 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/2008_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_2008 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_2008 <- pres_results_2008[grep("state/district", pres_results_2008, ignore.case = T)]
pres_results_2008 <- html_table(pres_results_2008[grep("state/district", pres_results_2008, ignore.case = T)], fill = T)[[1]]

head(pres_results_2008)

# RENAME COLUMNS
pres_results_2008 <- pres_results_2008 %>% setNames(c('state', 'ev',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'independent_total', 'independent_share', 'independent_votes',
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes',
                                                      'constitution_total', 'constitution_share', 'constitution_votes',
                                                      'green_total', 'green_share', 'green_votes',
                                                      'other_total', 'other_share', 'other_votes',
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_2008 <- pres_results_2008[c(-1, -58), c(-2, -5, -8, -11, -14, -17, -20, -23:-25)]
pres_results_2008$state[pres_results_2008$state == 'D.C.'] <- 'District of Columbia'
pres_results_2008$state[pres_results_2008$state == 'Maine†'] <- 'Maine'
pres_results_2008$state[pres_results_2008$state == "Maine's 1st"] <- 'Maine CD-1'
pres_results_2008$state[pres_results_2008$state == "Maine's 2nd"] <- 'Maine CD-2'
pres_results_2008$state[pres_results_2008$state == 'Nebraska†'] <- 'Nebraska'
pres_results_2008$state[pres_results_2008$state == "Nebraska's 1st"] <- 'Nebraska CD-1'
pres_results_2008$state[pres_results_2008$state == "Nebraska's 2nd"] <- 'Nebraska CD-2'
pres_results_2008$state[pres_results_2008$state == "Nebraska's 3rd"] <- 'Nebraska CD-3'

pres_results_2008[pres_results_2008 == ""] <- 0

pres_results_2008 <- pres_results_2008 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'independent_total', 'libertarian_total', 'constitution_total', 'green_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'independent_total', 'libertarian_total', 'constitution_total', 'green_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         independent_total = as.numeric(independent_total),
         constitution_total = as.numeric(constitution_total),
         libertarian_total = as.numeric(libertarian_total),
         green_total = as.numeric(green_total),
         other_total = as.numeric(other_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         independent_share = independent_total / total_votes,
         constitution_share = constitution_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         green_share = green_total / total_votes,
         other_share = other_total / total_votes)

head(pres_results_2008)

pres_res_2008 <- pres_results_2008 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, independent = independent_total, constitution = constitution_total, libertarian = libertarian_total, green = green_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_2008 <- pres_results_2008 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, independent = independent_share, constitution = constitution_share, libertarian = libertarian_share, green = green_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_2008) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'independent', 'constitution', 'libertarian', 'green', 'other')),
         election_day = ymd(20081104),
         incumbant_party = 'rep',
         winning_party = 'dem') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 2008)

pres_results_2008 <- pres_results_2008[, c(11, 1:10)]

dat_NAT <- pres_results_2008 %>%
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

pres_results_2008 <- pres_results_2008 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_2008) %>%
  arrange(cycle)



# 2004 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/2004_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_2004 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_2004 <- pres_results_2004[grep("state total", pres_results_2004, ignore.case = T)]
pres_results_2004 <- html_table(pres_results_2004[grep("state total", pres_results_2004, ignore.case = T)], fill = T)[[1]]

head(pres_results_2004)

# RENAME COLUMNS
pres_results_2004 <- pres_results_2004 %>% setNames(c('state', 'ev',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'independent_reform_total', 'independent_reform_share', 'independent_reform_votes',
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes',
                                                      'constitution_total', 'constitution_share', 'constitution_votes',
                                                      'green_total', 'green_share', 'green_votes',
                                                      'other_total', 'other_share', 'other_votes',
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

pres_results_2004$state[pres_results_2004$state == 'Maine★'] <- 'Maine'
pres_results_2004$state[pres_results_2004$state == 'Nebraska★'] <- 'Nebraska'

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_2004 <- pres_results_2004[c(-1, -53), c(-2, -5, -8, -11, -14, -17, -20, -23:-25)]
pres_results_2004$state[pres_results_2004$state == 'D.C.'] <- 'District of Columbia'

pres_results_2004 <- pres_results_2004 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'independent_reform_total', 'libertarian_total', 'constitution_total', 'green_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'independent_reform_total', 'libertarian_total', 'constitution_total', 'green_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         independent_reform_total = as.numeric(independent_reform_total),
         constitution_total = as.numeric(constitution_total),
         libertarian_total = as.numeric(libertarian_total),
         green_total = as.numeric(green_total),
         other_total = as.numeric(other_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         independent_reform_share = independent_reform_total / total_votes,
         constitution_share = constitution_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         green_share = green_total / total_votes,
         other_share = other_total / total_votes)

head(pres_results_2004)

pres_res_2004 <- pres_results_2004 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, independent_reform = independent_reform_total, constitution = constitution_total,
         libertarian = libertarian_total, green = green_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_2004 <- pres_results_2004 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, independent_reform = independent_reform_share, constitution = constitution_share,
         libertarian = libertarian_share, green = green_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_2004) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'independent_reform', 'constitution', 'libertarian', 'green', 'other')),
         election_day = ymd(20041102),
         incumbant_party = 'rep',
         winning_party = 'rep') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 2004)

pres_results_2004 <- pres_results_2004[, c(11, 1:10)]

dat_NAT <- pres_results_2004 %>%
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

pres_results_2004 <- pres_results_2004 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_2004) %>%
  arrange(cycle)



# 2000 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/2000_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_2000 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_2000 <- pres_results_2000[grep("state total", pres_results_2000, ignore.case = T)]
pres_results_2000 <- html_table(pres_results_2000[grep("state total", pres_results_2000, ignore.case = T)], fill = T)[[1]]

head(pres_results_2000)

# RENAME COLUMNS
pres_results_2000 <- pres_results_2000 %>% setNames(c('state', 'ev',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'green_total', 'green_share', 'green_votes',
                                                      'reform_total', 'reform_share', 'reform_votes',
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes',
                                                      'constitution_total', 'constitution_share', 'constitution_votes',
                                                      'natural_law_total', 'natural_law_share', 'natural_law_votes',
                                                      'other_total', 'other_share', 'other_votes',
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

pres_results_2000$state[pres_results_2000$state == 'Arizona*'] <- 'Arizona'
pres_results_2000$state[pres_results_2000$state == 'Maine†'] <- 'Maine'
pres_results_2000$state[pres_results_2000$state == 'Nebraska†'] <- 'Nebraska'

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_2000 <- pres_results_2000[c(-1, -53), c(-2, -5, -8, -11, -14, -17, -20, -23, -26:-28)]
pres_results_2000$state[pres_results_2000$state == 'D.C.'] <- 'District of Columbia'

pres_results_2000 <- pres_results_2000 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'green_total', 'libertarian_total', 'constitution_total', 'natural_law_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'green_total', 'libertarian_total', 'constitution_total', 'natural_law_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         green_total = as.numeric(green_total),
         libertarian_total = as.numeric(libertarian_total),
         constitution_total = as.numeric(constitution_total),
         natural_law_total = as.numeric(natural_law_total),
         other_total = as.numeric(other_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         green_share = green_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         constitution_share = constitution_total / total_votes,
         natural_law_share = natural_law_total / total_votes,
         other_share = other_total / total_votes)

head(pres_results_2000)

pres_res_2000 <- pres_results_2000 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, green = green_total, libertarian = libertarian_total, constitution = constitution_total, natural_law = natural_law_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_2000 <- pres_results_2000 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, green = green_share, libertarian = libertarian_share, constitution = constitution_share, natural_law = natural_law_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_2000) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'green', 'libertarian', 'constitution', 'natural_law', 'other')),
         election_day = ymd(20001107),
         incumbant_party = 'dem',
         winning_party = 'rep') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 2000)

pres_results_2000 <- pres_results_2000[, c(11, 1:10)]

dat_NAT <- pres_results_2000 %>%
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

pres_results_2000 <- pres_results_2000 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_2000) %>%
  arrange(cycle)



# 1996 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1996_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1996 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1996 <- pres_results_1996[grep("state total", pres_results_1996, ignore.case = T)]
pres_results_1996 <- html_table(pres_results_1996[grep("state total", pres_results_1996, ignore.case = T)], fill = T)[[1]]

head(pres_results_1996)

# RENAME COLUMNS
pres_results_1996 <- pres_results_1996 %>% setNames(c('state', 'ev',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'reform_total', 'reform_share', 'reform_votes',
                                                      'green_total', 'green_share', 'green_votes',
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes',
                                                      'other_total', 'other_share', 'other_votes',
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1996 <- pres_results_1996[c(-1, -53), c(-2, -5, -8, -11, -14, -17, -20:-22)]
pres_results_1996$state[pres_results_1996$state == 'D.C.'] <- 'District of Columbia'

pres_results_1996 <- pres_results_1996 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'reform_total', 'green_total', 'libertarian_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'reform_total', 'green_total', 'libertarian_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         reform_total = as.numeric(reform_total),
         green_total = as.numeric(green_total),
         libertarian_total = as.numeric(libertarian_total),
         other_total = as.numeric(other_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         reform_share = reform_total / total_votes,
         green_share = green_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         other_share = other_total / total_votes)

head(pres_results_1996)

pres_res_1996 <- pres_results_1996 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, reform = reform_total, green = green_total, libertarian = libertarian_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1996 <- pres_results_1996 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, reform = reform_share, green = green_share, libertarian = libertarian_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1996) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'reform', 'green', 'libertarian', 'other')),
         election_day = ymd(19961105),
         incumbant_party = 'dem',
         winning_party = 'dem') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1996)

pres_results_1996 <- pres_results_1996[, c(11, 1:10)]

dat_NAT <- pres_results_1996 %>%
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

pres_results_1996 <- pres_results_1996 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1996) %>%
  arrange(cycle)



# 1992 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1992_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1992 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1992 <- pres_results_1992[grep("state total", pres_results_1992, ignore.case = T)]
pres_results_1992 <- html_table(pres_results_1992[grep("state total", pres_results_1992, ignore.case = T)], fill = T)[[1]]

head(pres_results_1992)

# RENAME COLUMNS
pres_results_1992 <- pres_results_1992 %>% setNames(c('state', 'ev',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'indepenent_total', 'indepenent_share',
                                                      'libertarian_total', 'libertarian_share',
                                                      'other_total', 'other_share',
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1992 <- pres_results_1992[c(-1:-2, -54), c(-2, -5, -8, -15:-16)]
pres_results_1992$state[pres_results_1992$state == 'D.C.'] <- 'District of Columbia'

pres_results_1992 <- pres_results_1992 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'indepenent_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'indepenent_total', 'other_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         libertarian_total = as.numeric(libertarian_total),
         indepenent_total = as.numeric(indepenent_total),
         other_total = as.numeric(other_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         indepenent_share = indepenent_total / total_votes,
         other_share = other_total / total_votes)

head(pres_results_1992)

pres_res_1992 <- pres_results_1992 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, libertarian = libertarian_total, independent = indepenent_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1992 <- pres_results_1992 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, libertarian = libertarian_share, independent = indepenent_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1992) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'libertarian', 'independent', 'other')),
         election_day = ymd(19921103),
         incumbant_party = 'rep',
         winning_party = 'dem') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1992)

pres_results_1992 <- pres_results_1992[, c(11, 1:10)]

dat_NAT <- pres_results_1992 %>%
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

pres_results_1992 <- pres_results_1992 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1992) %>%
  arrange(cycle)



# 1988 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1988_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1988 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1988 <- pres_results_1988[grep("state total", pres_results_1988, ignore.case = T)]
pres_results_1988 <- html_table(pres_results_1988[grep("state total", pres_results_1988, ignore.case = T)], fill = T)[[1]]

head(pres_results_1988)

# RENAME COLUMNS
pres_results_1988 <- pres_results_1988 %>% setNames(c('state', 'ev',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes',
                                                      'new_alliance_total', 'new_alliance_share', 'new_alliance_votes',
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1988 <- pres_results_1988[c(-1, -53), c(-2, -5, -8, -11, -14:-16)]
pres_results_1988$state[pres_results_1988$state == 'D.C.'] <- 'District of Columbia'

pres_results_1988 <- pres_results_1988 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'new_alliance_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'new_alliance_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         libertarian_total = as.numeric(libertarian_total),
         new_alliance_total = as.numeric(new_alliance_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         new_alliance_share = new_alliance_total / total_votes,
         other_total = total_votes - (rep_total + dem_total + libertarian_total + new_alliance_total),
         other_share = other_total / total_votes)

head(pres_results_1988)

pres_res_1988 <- pres_results_1988 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, libertarian = libertarian_total, new_alliance = new_alliance_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1988 <- pres_results_1988 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, libertarian = libertarian_share, new_alliance = new_alliance_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1988) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'libertarian', 'new_alliance', 'other')),
         election_day = ymd(19881108),
         incumbant_party = 'rep',
         winning_party = 'rep') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1988)

pres_results_1988 <- pres_results_1988[, c(11, 1:10)]

dat_NAT <- pres_results_1988 %>%
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

pres_results_1988 <- pres_results_1988 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1988) %>%
  arrange(cycle)



# 1984 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1984_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1984 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1984 <- pres_results_1984[grep("state total", pres_results_1984, ignore.case = T)]
pres_results_1984 <- html_table(pres_results_1984[grep("state total", pres_results_1984, ignore.case = T)], fill = T)[[1]]

head(pres_results_1984)

# RENAME COLUMNS
pres_results_1984 <- pres_results_1984 %>% setNames(c('state', 'ev',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes', 
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1984 <- pres_results_1984[c(-1, -53), c(-2, -5, -8, -11:-13)]
pres_results_1984$state[pres_results_1984$state == 'D.C.'] <- 'District of Columbia'

pres_results_1984 <- pres_results_1984 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'libertarian_total', 'total_votes'),
            .funs = gsub,
            pattern = '–',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         libertarian_total = as.numeric(libertarian_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         other_total = total_votes - (rep_total + dem_total + libertarian_total),
         other_share = other_total / total_votes)

head(pres_results_1984)

pres_res_1984 <- pres_results_1984 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, libertarian = libertarian_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1984 <- pres_results_1984 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, libertarian = libertarian_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1984) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'libertarian', 'other')),
         election_day = ymd(19841106),
         incumbant_party = 'rep',
         winning_party = 'rep') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1984)

pres_results_1984 <- pres_results_1984[, c(11, 1:10)]

dat_NAT <- pres_results_1984 %>%
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

pres_results_1984 <- pres_results_1984 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1984) %>%
  arrange(cycle)



# 1980 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1980_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1980 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1980 <- pres_results_1980[grep("state total", pres_results_1980, ignore.case = T)]
pres_results_1980 <- html_table(pres_results_1980[grep("state total", pres_results_1980, ignore.case = T)], fill = T)[[1]]

head(pres_results_1980)

# RENAME COLUMNS
pres_results_1980 <- pres_results_1980 %>% setNames(c('state', 'ev',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'independent_total', 'independent_share', 'independent_votes', 
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes', 
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1980 <- pres_results_1980[c(-1, -53), c(-2, -5, -8, -11, -14:-16)]
pres_results_1980$state[pres_results_1980$state == 'D.C.'] <- 'District of Columbia'

pres_results_1980 <- pres_results_1980 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'independent_total', 'libertarian_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'independent_total', 'libertarian_total', 'total_votes'),
            .funs = gsub,
            pattern = '-',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         independent_total = as.numeric(independent_total),
         libertarian_total = as.numeric(libertarian_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         independent_share = independent_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         other_total = total_votes - (rep_total + dem_total + independent_total + libertarian_total),
         other_share = other_total / total_votes)

head(pres_results_1980)

pres_res_1980 <- pres_results_1980 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, independent = independent_total, libertarian = libertarian_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1980 <- pres_results_1980 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, independent = independent_share, libertarian = libertarian_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1980) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'independent', 'libertarian', 'other')),
         election_day = ymd(19801104),
         incumbant_party = 'dem',
         winning_party = 'rep') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1980)

pres_results_1980 <- pres_results_1980[, c(11, 1:10)]

dat_NAT <- pres_results_1980 %>%
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

pres_results_1980 <- pres_results_1980 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1980) %>%
  arrange(cycle)



# 1976 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1976_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1976 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1976 <- pres_results_1976[grep("state total", pres_results_1976, ignore.case = T)]
pres_results_1976 <- html_table(pres_results_1976[grep("state total", pres_results_1976, ignore.case = T)], fill = T)[[1]]

head(pres_results_1976)

# RENAME COLUMNS
pres_results_1976 <- pres_results_1976 %>% setNames(c('state', 'ev',
                                                      'dem_total', 'dem_share', 'dem_votes',
                                                      'rep_total', 'rep_share', 'rep_votes',
                                                      'independent_total', 'independent_share', 'independent_votes', 
                                                      'libertarian_total', 'libertarian_share', 'libertarian_votes', 
                                                      'matgin_votes', 'margin_share', 
                                                      'total_votes', 'state_abb'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_1976 <- pres_results_1976[c(-1, -53), c(-2, -5, -8, -11, -14:-16)]
pres_results_1976$state[pres_results_1976$state == 'D.C.'] <- 'District of Columbia'

pres_results_1976 <- pres_results_1976 %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'independent_total', 'libertarian_total', 'total_votes'),
            .funs = gsub,
            pattern = ',',
            replacement = '') %>%
  mutate_at(.vars = c('rep_total', 'dem_total', 'independent_total', 'libertarian_total', 'total_votes'),
            .funs = gsub,
            pattern = '-',
            replacement = '0') %>%
  mutate(rep_total = as.numeric(rep_total),
         dem_total = as.numeric(dem_total),
         independent_total = as.numeric(independent_total),
         libertarian_total = as.numeric(libertarian_total),
         total_votes = as.numeric(total_votes),
         rep_share = rep_total / total_votes,
         dem_share = dem_total / total_votes,
         independent_share = independent_total / total_votes,
         libertarian_share = libertarian_total / total_votes,
         other_total = total_votes - (rep_total + dem_total + independent_total + libertarian_total),
         other_share = other_total / total_votes)

head(pres_results_1976)

pres_res_1976 <- pres_results_1976 %>%
  select(state, state_abb, rep = rep_total, dem = dem_total, independent = independent_total, libertarian = libertarian_total, other = other_total) %>%
  gather(party, total_votes, rep:other)

pres_results_1976 <- pres_results_1976 %>%
  select(state, state_abb, rep = rep_share, dem = dem_share, independent = independent_share, libertarian = libertarian_share, other = other_share) %>%
  gather(party, vote_share, rep:other) %>%
  left_join(pres_res_1976) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'independent', 'libertarian', 'other')),
         election_day = ymd(19761102),
         incumbant_party = 'rep',
         winning_party = 'dem') %>%
  group_by(state) %>%
  arrange(state, party) %>%
  mutate(state_winner = ifelse(vote_share[1] > vote_share[2], 'dem', 'rep'),
         run_date = RUN_DATE) %>%
  arrange(state, party) %>%
  mutate(cycle = 1976)

pres_results_1976 <- pres_results_1976[, c(11, 1:10)]

dat_NAT <- pres_results_1976 %>%
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

pres_results_1976 <- pres_results_1976 %>%
  rbind(dat_NAT) %>%
  arrange(cycle) %>%
  mutate(party = as.character(party))

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1976) %>%
  arrange(cycle)


# 1972 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1972_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1972 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1972 <- pres_results_1972[grep("Outcomes of the 1972 United States presidential election by state", pres_results_1972, ignore.case = T)]
pres_results_1972 <- html_table(pres_results_1972[grep("Outcomes of the 1972 United States presidential election by state", pres_results_1972, ignore.case = T)], fill = T)[[1]]

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

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1972) %>%
  arrange(cycle)



# 1968 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1968_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1968 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1968 <- pres_results_1968[grep("State Total", pres_results_1968, ignore.case = T)]
pres_results_1968 <- html_table(pres_results_1968[grep("State Total", pres_results_1968, ignore.case = T)], fill = T)[[1]]

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

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1968) %>%
  arrange(cycle)



# 1964 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1964_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1964 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1964 <- pres_results_1964[grep("State Total", pres_results_1964, ignore.case = T)]
pres_results_1964 <- html_table(pres_results_1964[grep("State Total", pres_results_1964, ignore.case = T)], fill = T)[[1]]

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

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1964) %>%
  arrange(cycle)



# 1960 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1960_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1960 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1960 <- pres_results_1960[grep("State Total", pres_results_1960, ignore.case = T)]
pres_results_1960 <- html_table(pres_results_1960[grep("State Total", pres_results_1960, ignore.case = T)], fill = T)[[1]]

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

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1960) %>%
  arrange(cycle)



# 1956 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1956_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1956 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1956 <- pres_results_1956[grep("State Total", pres_results_1956, ignore.case = T)]
pres_results_1956 <- html_table(pres_results_1956[grep("State Total", pres_results_1956, ignore.case = T)], fill = T)[[1]]

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

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1956) %>%
  arrange(cycle)



# 1952 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1952_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1952 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1952 <- pres_results_1952[grep("State Total", pres_results_1952, ignore.case = T)]
pres_results_1952 <- html_table(pres_results_1952[grep("State Total", pres_results_1952, ignore.case = T)], fill = T)[[1]]

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

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1952) %>%
  arrange(cycle)



# 1948 -------------------------------------------------------------------------
url <- 'https://en.wikipedia.org/wiki/1948_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_1948 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_1948 <- pres_results_1948[grep("State Total", pres_results_1948, ignore.case = T)]
pres_results_1948 <- html_table(pres_results_1948[grep("State Total", pres_results_1948, ignore.case = T)], fill = T)[[1]]

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

pres_results_1948$state[pres_results_1948$state == 'Mississippi[b][88]'] <- 'Mississippi'
pres_results_1948$state[pres_results_1948$state == 'New York[a][88]'] <- 'New York'

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

potus_results_46_20 <- potus_results_46_20 %>%
  rbind(pres_results_1948) %>%
  arrange(cycle) %>%
  mutate(run_date = RUN_DATE)



# Final Tidy -------------------------------------------------------------------
write.csv(potus_results_46_20, 'data/potus_results_46_20.csv')

## Build Dem vs Rep vs Other only table
# Only Dems and Reps
potus_results_46_20_DRO <- potus_results_46_20 %>%
  filter(party %in% c('dem', 'rep'))

head(potus_results_46_20_DRO)

# Total votes per state
dat_RES <- potus_results_46_20 %>%
  group_by(cycle, state) %>%
  summarize(all_votes = sum(total_votes))

# Other
dat_OTHER <- potus_results_46_20 %>%
  left_join(dat_RES) %>%
  filter(!party %in% c('dem', 'rep')) %>%
  group_by(cycle, state) %>%
  summarize(state_abb = state_abb[1],
            party = 'other',
            total_votes = sum(total_votes),
            vote_share = total_votes / all_votes[1],
            election_day = election_day[1],
            incumbant_party = incumbant_party[1],
            winning_party = winning_party[1],
            state_winner = state_winner[1],
            run_date = RUN_DATE) %>%
  select(cycle, state, state_abb, party, vote_share, total_votes, election_day, incumbant_party, winning_party, state_winner, run_date)

# Bind
potus_results_46_20_dem_rep_other <- potus_results_46_20_DRO %>%
  rbind(dat_OTHER) %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'other'))) %>%
  arrange(cycle, state, party)

write.csv(potus_results_46_20_dem_rep_other, 'data/potus_results_46_20_dem_rep_other.csv')



