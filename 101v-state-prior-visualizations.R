{# Libraries
  library(tidyverse)
  library(tidylog)
  library(lubridate)
  library(broom)
  library(knitr)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}
options(scipen = 999)


# Master Variables -------------------------------------------------------------
# Graph Colors
PARTY_COL <- c('darkblue', 'red4', 'green4')
PARTY_LEV <- c('dem', 'rep', 'other')

# Data Load --------------------------------------------------------------------
load('rda/pres_results_prior.rda')
load('rda/modeled_EV_state_outcome_party.rda')

# Visualize All State Trends ALL PARTIES
pres_results_prior %>%
  ggplot(aes(x = cycle, y = vote_share, color = party)) +
  geom_point() +
  scale_color_manual(values = PARTY_COL) +
  scale_x_continuous(breaks = seq(1972, 2028, 4)) +
  geom_smooth(method = 'lm') +
  facet_wrap(. ~ state) +
  theme(axis.text.x = element_text(angle = 90))

# Visualize One State Trend ALL PARTIES
pres_results_prior %>%
  filter(state == 'Florida') %>%
  ggplot(aes(x = cycle, y = vote_share, color = party)) +
  geom_point() +
  scale_color_manual(values = party_col) +
  scale_x_continuous(breaks = seq(1972, 2028, 4)) +
  geom_smooth(method = 'lm') +
  theme(axis.text.x = element_text(angle = 90))

# Visualize Spread Trends (Dem vs Rep Only)
pres_results_prior %>%
  group_by(cycle, state) %>%
  summarize(spread = vote_share[2] - vote_share[1]) %>%
  mutate(party = ifelse(spread < 0, 'dem', 'rep')) %>%
  ggplot(aes(x = cycle, y = spread)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color = party)) +
  scale_color_manual(values = PARTY_COL) +
  geom_smooth(method = 'lm') +
  coord_flip() +
  scale_x_reverse(breaks = seq(1972, 2028, 4)) +
  facet_wrap(. ~ state) +
  theme(axis.text.x = element_text(angle = 90))




head(modeled_EV_state_outcome_party)

modeled_EV_state_outcome_party %>%
  mutate(party = factor(party, levels = rev(PARTY_LEV))) %>%
  ggplot(aes(x = state, y = vote_share_mu, ymin = vote_share_start, ymax = vote_share_end, color = party, label = party)) +
  geom_hline(yintercept = 0) +
  geom_point(position = position_dodge(width = 0.90)) +
  geom_errorbar(position = 'dodge') +
#  geom_text(aes(y = vote_share_end), position = position_dodge(width = 0.90), hjust = 1) +
  scale_color_manual(values = rev(PARTY_COL)) +
  coord_flip() +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank()) +
  facet_wrap(. ~ state, scales = 'free_y')

