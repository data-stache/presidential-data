head(dat_POLLS)
head(modeled_EV_state_spreads_c2020)
head(modeled_EV_state_outcome_party_2020)

dat_PRIOR <- modeled_EV_state_spreads_c2020 %>%
  rename(prior_mu = spread_mu, prior_se = spread_se) %>%
  select(state:prior_se)

dat_PRIOR_PARTY <- modeled_EV_state_outcome_party_2020 %>% 
  filter(!party == 'other')  %>%
  select(state:vote_share_se)

dat_POLLS %>%
  rename(poll_avg = spread, poll_sd = spread_sd) %>%
  select(state:poll_sd) %>%
  left_join(dat_PRIOR) %>%
  mutate(B = poll_sd^2 / (poll_sd^2 + prior_se^2),
         posterior_mean = B * prior_mu + (1 - B) * poll_avg,
         posterior_sd = sqrt(1 / (1/poll_sd^2 + 1/prior_se^2)),
         conf_int = 2 * posterior_sd,
         start = posterior_mean - conf_int,
         end = posterior_mean + conf_int) %>%
  left_join(dat_NAT) %>%
  mutate(state = factor(state, levels = ord)) %>%
  ggplot(aes(x = state, y = posterior_mean, ymin = start, ymax = end)) +
  geom_hline(yintercept = 0) +
  geom_errorbar() +
  geom_point() +
  geom_point(aes(y = spread_2020), color = 'red') +
  coord_flip()
  
  


mu <- 0
tau <- 0.02
bias_sd <- 0.03 #for state level we assum larger bias so set to 3%
clinton_EV_3 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     z = qt(0.975, n),
                     t_dist = z * sd / sqrt(n),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, t_dist),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state