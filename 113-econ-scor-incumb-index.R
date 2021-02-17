load('rda/state_econ_score.rda')
load('rda/pres_results_prior.rda')
load('rda/NDIs_SPREAD.rda')

head(NDIs_SPREAD)
NDIs <- NDIs_SPREAD %>%
  select(cycle, state, state_abb, expected_NDI)

head(econ)
econ_score <- econ %>%
  filter(var == 'SAEI')

head(pres_results_prior)
national_spreads <- pres_results_prior %>%
  filter(state == 'National') %>%
  group_by(cycle, state) %>%
  mutate(national_spread = vote_share[2] - vote_share[1]) %>%
  slice_head(n= 1) %>%
  ungroup() %>%
  select(cycle, national_spread)

inc_spreads <- pres_results_prior %>%
  select(-cycle_weight, -total_votes, -winning_party, -state_winner, -run_date) %>%
  left_join(national_spreads) %>%
  group_by(cycle, state) %>%
  mutate(state_spread = vote_share[2] - vote_share[1],
         inc_spread = ifelse(incumbant_party == 'dem', vote_share[1] - vote_share[2], vote_share[2] - vote_share[1]),
         nat_inc_dev_index = inc_spread - national_spread) %>%
  slice_head(n = 1) %>%
  select(-party:-vote_share) %>%
  ungroup() %>%
  group_by(state)

head(inc_spreads)
head(econ_score)

min_econ_year <- year(min(econ_score$date))

# Years with Econ Data
inc_spreads <- inc_spreads %>%
  filter(cycle >= min_econ_year + 4)

election_years <- unique(inc_spreads$cycle)
inn_days <- paste(election_years-3, '0120', sep = '')
inn_days <- ymd(inn_days)

election_days <- ymd(unique(inc_spreads$election_day))

# Cycles with Econ Data
econ_score <- econ_score %>%
  filter(date >= inn_days[1])

CYCLES <- year(election_days)

econ_score$cycle <- 0

for (i in 1:length(CYCLES)) {
  econ_score$cycle <- ifelse(econ_score$date %in% seq(inn_days[i], election_days[i], 'day'), CYCLES[i], econ_score$cycle)
}

cycles_eday <- data.frame(cycle = CYCLES,
                          election_day = election_days)

econ_score <- econ_score %>%
  left_join(cycles_eday)

econ_score_cycle <- map_df(CYCLES, function(C) {
  econ_score %>%
    filter(cycle == C) %>%
    group_by(state) %>%
    mutate(weight = case_when(year(date) == year(election_day) - 3 ~ .1,
                              year(date) == year(election_day) - 2 ~ .2,
                              year(date) == year(election_day) - 1 ~ .3,
                              year(date) == year(election_day) ~ .4)) %>%
    dplyr::summarize(avg_score = weighted.mean(score, weight),
              cycle = cycle[1]) %>%
    select(state_abb = state, cycle, avg_score)
})

inc_spreads %>%
  left_join(NDIs) %>%
  mutate(deviation_shift = inc_spread - expected_NDI) %>%
  left_join(econ_score_cycle) %>%
  ggplot(aes(x = avg_score, y = deviation_shift, label = cycle)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(. ~ state)


inc_spreads %>%
  left_join(econ_score_cycle) %>%
  arrange(cycle)











