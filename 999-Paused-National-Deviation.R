{# Libraries
  library(Hmisc, quietly = TRUE)
  library(lubridate, quietly = TRUE)
  library(broom, quietly = TRUE)
  library(knitr, quietly = TRUE)
  library(zoo, quietly = TRUE)
  library(tidyverse, quietly = TRUE)
  library(tidylog, quietly = TRUE)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}

# Master Variables -------------------------------------------------------------
RUN_DATE <- Sys.Date()
CURRENT_CYCLE <- 2024

# Graph Colors
PARTY_COL <- c('darkblue', 'red', 'green4')
PARTY_LEV <- c('dem', 'rep', 'other')


# Load PResidential Election Data
load('rda/pres_results_prior.rda')
dat_PRES <- pres_results_prior %>%
  select(cycle, state, state_abb, party, vote_share)

head(dat_PRES)

# Load House Data
house_results_all <- politicaldata::house_results

# Assign NAs to 0
{
  house_results_all$dem[is.na(house_results_all$dem)] <- 0
  house_results_all$rep[is.na(house_results_all$rep)] <- 0
  house_results_all$other[is.na(house_results_all$other)] <- 0
  house_results_all$total_votes[is.na(house_results_all$total_votes)] <- 0
}

# Assign State Level House Values
house_state_shares <- house_results_all %>%
  mutate(dem = dem * total_votes,
         rep = rep * total_votes,
         other = other * total_votes) %>%
  group_by(year, state_abb) %>%
  summarize(dem = sum(dem),
            rep = sum(rep),
            other = sum(other)) %>%
  ungroup() %>%
  gather(party, total_votes, dem:other) %>%
  group_by(year, state_abb) %>%
  mutate(party = factor(party, levels = PARTY_LEV),
         share = total_votes / sum(total_votes)) %>%
  arrange(year, state_abb, party) %>%
  select(cycle = year, state_abb, party, share)

# Assign National Level Values
house_nat_shares <- house_results_all %>%
  mutate(dem = dem * total_votes,
         rep = rep * total_votes,
         other = other * total_votes) %>%
  group_by(year) %>%
  summarize(dem = sum(dem),
            rep = sum(rep),
            other = sum(other)) %>%
  ungroup() %>%
  gather(party, total_votes, dem:other) %>%
  group_by(year) %>%
  mutate(party = factor(party, levels = PARTY_LEV),
         nat_share = total_votes / sum(total_votes)) %>%
  arrange(year) %>%
  mutate(state_abb = 'NAT') %>%
  select(cycle = year, party, nat_share)

house_deviation <- house_state_shares %>%
  left_join(house_nat_shares) %>%
  filter(!party == 'other') %>%
  mutate(national_deviation = nat_share - share,
         national_deviation = ifelse(party == 'rep', national_deviation * -1, national_deviation)) %>%
  group_by(state_abb, party) %>%
  mutate(national_dev_score = rollapply(national_deviation, width = 4, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup()


load('rda/pres_results_prior.rda')
dat_PRES <- pres_results_prior %>%
  select(cycle, state, state_abb, party, vote_share) %>%
  arrange(cycle)

dat_NAT <- dat_PRES %>%
  filter(state == 'National') %>%
  select(cycle, party, nat_pres = vote_share)

dat_PRES <- dat_PRES %>%
  filter(!state == 'National')

dat_PRES <- dat_PRES %>%
  left_join(dat_NAT) %>%
  filter(!party == 'other') %>%
  mutate(pres_deviation = nat_pres - vote_share,
         pres_deviation = ifelse(party == 'rep', pres_deviation * -1, pres_deviation)) %>%
  group_by(state, party) %>%
  mutate(pres_deviation_score = rollapply(pres_deviation, width = 2, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup()

head(house_deviation)
head(dat_PRES)

deviation <- house_deviation %>%
  left_join(dat_PRES) %>%
  mutate(dev_score = ifelse(is.na(pres_deviation), national_deviation, (national_deviation + pres_deviation) / 2)) %>%
  select(cycle, state_abb, party, dev_score) %>%
  group_by(state_abb, party) %>%
  mutate(dev_score_avg = rollapply(dev_score, width = 4, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup() %>%
  mutate(cycle = cycle + 2)

cycles <- unique(dat_PRES$cycle)
cycles <- cycles[-1]

deviation %>%
  filter(cycle %in% cycles) %>%
  left_join(dat_PRES) %>%
  group_by(party) %>%
  mutate(dev_score = (dev_score - mean(dev_score)) / sd(dev_score)) %>%
  ungroup() %>%
  group_by(state, party) %>%
  mutate(change = (dev_score - lag(dev_score)) / lag(dev_score)) %>%
  ungroup() %>%
  filter(party == 'rep') %>%
  ggplot(aes(x = dev_score, y = vote_share, color = party)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_color_manual(values = PARTY_COL)


  
dat_DEV <- deviation %>%
  filter(cycle %in% cycles) %>%
  left_join(dat_PRES) %>%
  group_by(party) %>%
  mutate(dev_score = (dev_score - mean(dev_score)) / sd(dev_score)) %>%
  ungroup() %>%
  select(cycle, state, state_abb, party, vote_share, pres_deviation_score)

dat_DEV %>%
  filter(party == 'rep') %>%
  lm(vote_share ~ pres_deviation_score, data = .)

PARTIES <- c('dem', 'rep')


party_fit <- map_df(PARTIES, function(PY) {
  fit <- dat_DEV %>%
    filter(party == PY) %>%
    lm(vote_share ~ pres_deviation_score, data = .)
  tidy(fit)
})

fit <- dat_DEV %>%
  filter(party == 'rep') %>%
  lm(vote_share ~ pres_deviation_score, data = .)

dat_DEV %>%
  filter(state_abb == 'AZ' & party == 'dem') %>%
  arrange(desc(cycle))

dat_NAT

0.48397 + -0.0258 * 1.038




