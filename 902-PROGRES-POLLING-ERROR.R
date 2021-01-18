{# Libraries
  library(Hmisc, quietly = TRUE)
  library(lubridate, quietly = TRUE)
  library(broom, quietly = TRUE)
  library(knitr, quietly = TRUE)
  library(tidyverse, quietly = TRUE)
  library(tidylog, quietly = TRUE)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}
options(scipen = 999)



load('rda/pres_results_prior.rda')
head(pres_results_prior)

pres_results_prior <- pres_results_prior %>%
  select(cycle, state, state_abb, party, vote_share)

pres_gen_poll_error <- read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw-polls.csv', stringsAsFactors = FALSE, header = TRUE)

pres_gen_poll_error <- pres_gen_poll_error %>%
  filter(type_simple == 'Pres-G')

{
pres_gen_poll_error$location[pres_gen_poll_error$location == 'AK'] <- 'Alaska'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'AL'] <- 'Alabama'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'AZ'] <- 'Arizona'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'AR'] <- 'Arkansas'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'CA'] <- 'California'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'CO'] <- 'Colorado'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'CT'] <- 'Connecticut'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'DE'] <- 'Delaware'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'DC'] <- 'District of Columbia'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'FL'] <- 'Florida'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'GA'] <- 'Georgia'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'HI'] <- 'Hawaii'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'IA'] <- 'Iowa'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'ID'] <- 'Idaho'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'IL'] <- 'Illinois'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'IN'] <- 'Indiana'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'KS'] <- 'Kansas'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'KY'] <- 'Kentucky'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'LA'] <- 'Louisiana'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'ME'] <- 'Maine'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'M1'] <- 'Maine CD-1'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'M2'] <- 'Maine CD-2'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'MD'] <- 'Maryland'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'MA'] <- 'Massachusetts'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'MI'] <- 'Michigan'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'MN'] <- 'Minnesota'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'MS'] <- 'Mississippi'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'MO'] <- 'Missouri'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'MT'] <- 'Montana'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'NE'] <- 'Nebraska'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'N2'] <- 'Nebraska CD-2'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'NV'] <- 'Nevada'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'NH'] <- 'New Hampshire'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'NJ'] <- 'New Jersey'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'NM'] <- 'New Mexico'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'NY'] <- 'New York'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'NC'] <- 'North Carolina'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'ND'] <- 'North Dakota'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'OH'] <- 'Ohio'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'OK'] <- 'Oklahoma'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'OR'] <- 'Oregon'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'PA'] <- 'Pennsylvania'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'RI'] <- 'Rhode Island'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'SC'] <- 'South Carolina'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'SD'] <- 'South Dakota'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'TN'] <- 'Tennessee'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'TX'] <- 'Texas'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'US'] <- 'National'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'UT'] <- 'Utah'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'VT'] <- 'Vermont'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'VA'] <- 'Virginia'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'WA'] <- 'Washington'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'WV'] <- 'West Virginia'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'WI'] <- 'Wisconsin'
pres_gen_poll_error$location[pres_gen_poll_error$location == 'WY'] <- 'Wyoming'
}

dat_1 <- pres_gen_poll_error %>%
  rename(dem = cand1_pct, rep = cand2_pct, other = cand3_pct) %>%
  select(-cand1_name, -cand1_party , -cand2_name, -cand2_party, -comment, -partisan, -margin_poll, -cand1_actual:-rightcall) %>%
  gather(party, poll_share, dem:other) %>%
  mutate(poll_share = poll_share / 100) %>%
  arrange(desc(poll_id))

head(dat_1)

dat_2 <- pres_gen_poll_error %>%
  select(poll_id, location, cand1_actual:rightcall) %>%
  rename(dem = cand1_actual, rep = cand2_actual) %>%
  gather(party, actual_share, dem:rep) %>%
  mutate(actual_share = actual_share / 100) %>%
  select(-margin_actual:-rightcall) %>%
  arrange(desc(poll_id))

head(dat_2)

pres_gen_poll_error_tidy <- dat_1 %>%
  left_join(dat_2) %>%
  rename(cycle = year, state = location, poll_date = polldate, sample_size = samplesize, election_date = electiondate) %>%
  mutate(poll_date = mdy(poll_date),
         election_date = mdy(election_date)) %>%
  arrange(cycle)

pres_gen_poll_error_tidy$poll_share[is.na(pres_gen_poll_error_tidy$poll_share)] <- 0
pres_gen_poll_error_tidy$actual_share[is.na(pres_gen_poll_error_tidy$actual_share)] <- 0

pres_gen_poll_error_tidy <- pres_gen_poll_error_tidy %>%
  left_join(pres_results_prior) %>%
  select(-question_id, -race_id, -type_detail, -pollster_rating_id)

pres_gen_poll_error_tidy <- pres_gen_poll_error_tidy[, c(1, 9, 4, 13, 2:3, 5:8, 10:11, 14)]

pres_gen_poll_error_tidy <- pres_gen_poll_error_tidy %>%
  mutate(error = abs(vote_share - poll_share),
         bias = poll_share - vote_share)

pres_gen_poll_error_tidy %>%
  group_by(state, party) %>%
  summarise(error = mean(error, na.rm = TRUE),
            bias = mean(bias, na.rm = TRUE)) %>%
  kable()

pres_gen_poll_error_tidy %>%
  group_by(cycle, state, party) %>%
  summarise(error = mean(error, na.rm = TRUE),
            bias = mean(bias, na.rm = TRUE)) %>%
  ggplot(aes(x = cycle, y = error, color = party)) +
  scale_x_continuous(breaks = seq(200, 2016, 4)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point() +
  facet_wrap(. ~ state)


  

head(pres_gen_poll_error)
head(pres_gen_poll_error_tidy)










pres_gen_poll_error <- pres_gen_poll_error %>%
  rename(state = location) %>%
  mutate(error = error / 100) %>%
  group_by(year, state) %>%
  summarize(mean_error = mean(error)) %>%
  ungroup() %>%
  group_by(state) %>%
  summarize(state_error_mu = mean(mean_error),
            state_error_sd = sd(mean_error),
            state_error_N = n()) %>%
  ungroup() %>%
  mutate(state_error_sd = ifelse(is.na(state_error_sd), median(state_error_sd, na.rm = TRUE), state_error_sd),
         state_error_se = state_error_sd / sqrt(state_error_N))

pres_gen_poll_error %>%
  kable()

save(pres_gen_poll_error, file = 'rda/pres_gen_poll_error.rda')
            