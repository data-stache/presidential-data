{# Libraries
  library(tidyverse)
  library(tidylog)
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
load('rda/modeled_EV_state_spreads.rda')

# Visualize All State Trends ALL PARTIES
dat_red <- unique(pres_results_prior %>%
  filter(state_winner == 'rep' & cycle == 2020) %>%
  .$state)

dat_blue <- unique(pres_results_prior %>%
  filter(state_winner == 'dem' & cycle == 2020) %>%
  .$state)

pres_results_prior %>%
  filter(state %in% dat_red) %>%
  ggplot(aes(x = cycle, y = vote_share, color = party)) +
  geom_hline(yintercept = 0, color = 'grey60') +
  geom_point() +
  scale_color_manual(values = PARTY_COL) +
  scale_x_continuous(breaks = seq(1948, 2028, 4)) +
  geom_smooth(method = 'lm', se = FALSE) +
  ggtitle('Share of Votes Received by Party in a Presidential Election',
          subtitle = 'States that the Republican Party Won in 2020') +
  labs(caption = 'Created by Andrew F. Griffin - 2021') +
  facet_wrap(. ~ state) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90,
                                   size = rel(2)),
        axis.text.y = element_text(size = rel(2)))

p_width <- 9
p_height <- (9/16) * p_width 

ggsave("figs/state-election-trends-red.png",
       width = p_width,
       dpi = "retina")


pres_results_prior %>%
  filter(state %in% dat_blue) %>%
  ggplot(aes(x = cycle, y = vote_share, color = party)) +
  geom_hline(yintercept = 0, color = 'grey60') +
  geom_point() +
  geom_line() +
  scale_color_manual(values = PARTY_COL) +
  scale_x_continuous(breaks = seq(1948, 2028, 4)) +
  geom_smooth(method = 'lm', se = FALSE) +
  ggtitle('Share of Votes Received by Party in a Presidential Election',
          subtitle = 'States that the Democratic Party Won in 2020') +
  labs(caption = 'Created by Andrew F. Griffin - 2021') +
  facet_wrap(. ~ state) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90,
                                   size = rel(2)),
        axis.text.y = element_text(size = rel(2)))

ggsave("figs/state-election-trends-blue.png",
       width = p_width,
       dpi = "retina")



# Visualize One State Trend ALL PARTIES
pres_results_prior %>%
  filter(state == 'Florida') %>%
  ggplot(aes(x = cycle, y = vote_share, color = party)) +
  geom_point() +
  scale_color_manual(values = PARTY_COL) +
  scale_x_continuous(breaks = seq(1972, 2028, 4)) +
  geom_smooth(method = 'lm') +
  theme(axis.text.x = element_text(angle = 90))

# Visualize Spread Trends (Dem vs Rep Only)
pres_results_prior %>%
  group_by(cycle, state) %>%
  summarize(spread = vote_share[2] - vote_share[1],
            cycle_weight = cycle_weight[1]) %>%
  mutate(party = ifelse(spread < 0, 'dem', 'rep')) %>%
  filter(!state == 'National') %>%
  ggplot(aes(x = cycle, y = spread)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color = party)) +
  geom_smooth(aes(weight = cycle_weight), method = 'lm', se = FALSE) +
  scale_color_manual(values = PARTY_COL) +
  coord_flip() +
  scale_x_reverse(breaks = seq(1948, 2028, 4)) +
  scale_y_continuous(breaks = seq(-1, 1, .25)) +
  ggtitle('Spread of Presidential Elections since 1976 with Weighted Line of Best Fit') +
  labs(caption = 'Created by Andrew F. Griffin - 2021') +
  facet_wrap(. ~ state) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90,
                                   size = rel(2)),
        axis.text.y = element_text(size = rel(2)))

ggsave("figs/state-election-spread-trends-fit-weighted.png",
       width = p_width,
       dpi = "retina")

# Party Spread EVs
modeled_EV_state_outcome_party %>%
  mutate(party = factor(party, levels = rev(PARTY_LEV))) %>%
  filter(!state == 'National') %>%
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



# Visualize Forecasted Spread Trends (Dem vs Rep Only) -------------------------
head(modeled_EV_state_spreads)

vec_prior_ord <- modeled_EV_state_spreads %>%
  arrange(desc(spread_mu)) %>%
  .$state %>%
  unique(.)

prior_party <- c('red4', 'purple2', 'blue4')

modeled_EV_state_spreads %>%
  left_join(dat_2020) %>%
  mutate(party = case_when(spread_start > 0 & spread_end > 0 ~ 'rep',
                           spread_start < 0 & spread_end > 0 ~ 'toss',
                           spread_start < 0 & spread_end < 0 ~ 'dem'),
         party = factor(party, levels = c('rep', 'toss', 'dem')),
         state = factor(state, levels =  vec_prior_ord)) %>%
  ggplot(aes(x = state, y = spread, ymin = spread_start, ymax = spread_end, color = party, label = state)) +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = -0.1, ymax = 0.1, size = 0, fill = 'purple4', alpha = .2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-.1, .1), color = 'grey60', linetype = 2) +
  geom_errorbar() +
  geom_point(shape = 23, size = 2, fill = 'yellow', color = 'black') +
  scale_y_continuous(breaks = seq(-1, 1, .05)) +
  geom_text(aes(y = ifelse(state %in% c('Mississippi', 'Kansas', 'Louisiana', 'Nebraska', 'Nebraska CD-1', 'South Dakota', 'District of Columbia', 'Indiana', 'Nebraska CD-2', 'Montana', 'South Carolina', 'Missouri', 'Texas', 'Alaska', 'Georgia'), spread_end, spread_start)), 
            hjust = ifelse(modeled_EV_state_spreads$state %in% c('Mississippi', 'Kansas', 'Louisiana', 'Nebraska', 'Nebraska CD-1', 'South Dakota','District of Columbia', 'Indiana', 'Nebraska CD-2', 'Montana', 'South Carolina', 'Missouri', 'Texas', 'Alaska', 'Georgia'), 0, 1), 
            nudge_y = ifelse(modeled_EV_state_spreads$state %in% c('Mississippi', 'Kansas', 'Louisiana', 'Nebraska', 'Nebraska CD-1', 'South Dakota','District of Columbia', 'Indiana', 'Nebraska CD-2', 'Montana', 'South Carolina', 'Missouri', 'Texas', 'Alaska', 'Georgia'), .02, -.02),
            size = 2) +
  scale_color_manual(values = prior_party) +
  coord_flip() +
  ggtitle('Forecasted State Spreads vs Actual Outcome 2020',
          subtitle = 'Base Only on Linear Models of States Voting History') +
  labs(caption = 'Created by Andrew F. Griffin') +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90,
                                   size = rel(2)),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank())

ggsave("figs/state-election-spread-vs-2020-outcome-weighted.png",
       width = p_width,
       dpi = "retina")



# Visualize Forecasted Spread Trends (Dem vs Rep Only) - 2020 ------------------
head(modeled_EV_state_spreads_c2020)

vec_prior_ord <- modeled_EV_state_spreads_c2020 %>%
  arrange(desc(spread_mu)) %>%
  .$state

dat_2020 <- pres_results_prior %>%
  group_by(cycle, state) %>%
  summarize(spread = vote_share[2] - vote_share[1]) %>%
  mutate(party = ifelse(spread < 0, 'dem', 'rep')) %>%
  filter(cycle == 2020) %>%
  select(-party)

prior_party <- c('red4', 'purple2', 'blue4')

modeled_EV_state_spreads_c2020 %>%
  mutate(party = case_when(spread_start > 0 & spread_end > 0 ~ 'rep',
                           spread_start < 0 & spread_end > 0 ~ 'toss',
                           spread_start < 0 & spread_end < 0 ~ 'dem'),
         party = factor(party, levels = c('rep', 'toss', 'dem'))) %>%
  left_join(dat_2020) %>%
  mutate(state = factor(state, levels =  vec_prior_ord)) %>%
  ggplot(aes(x = state, y = spread_mu, ymin = spread_start, ymax = spread_end, color = party, label = state)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-.06, .06)) +
  geom_errorbar() +
  geom_point(aes(y = spread), color = 'red') +
  scale_y_continuous(breaks = seq(-1, 1, .02)) +
  geom_text(aes(y = ifelse(state %in% c('District of Columbia', 'Indiana', 'Nebraska CD-2', 'Montana', 'South Carolina', 'Missouri', 'Texas', 'Alaska'), spread_end, spread_start)), 
            hjust = ifelse(modeled_EV_state_spreads$state %in% c('District of Columbia', 'Indiana', 'Nebraska CD-2', 'Montana', 'South Carolina', 'Missouri', 'Texas', 'Alaska'), 0, 1), 
            nudge_y = ifelse(modeled_EV_state_spreads$state %in% c('District of Columbia', 'Indiana', 'Nebraska CD-2', 'Montana', 'South Carolina', 'Missouri', 'Texas', 'Alaska'), .02, -.02)) +
  scale_color_manual(values = prior_party) +
  coord_flip() +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank())



# Visualize Forecasted Spread Changes - 2024 DEMOCRAT --------------------------
head(modeled_EV_state_spreads_c2020)
head(modeled_EV_state_spreads)

vec_prior_ord <- modeled_EV_state_spreads_c2020 %>%
  arrange(desc(spread_mu)) %>%
  .$state

dat_01 <- data.frame(state = factor(vec_prior_ord, levels = vec_prior_ord),
           label = 2024) %>%
  mutate(label = paste(state, label))

dat_02 <- data.frame(state = factor(vec_prior_ord, levels = vec_prior_ord),
                     label = 2020) %>%
  mutate(label = paste(state, label))

label_ord <- dat_01 %>%
  rbind(dat_02) %>%
  arrange(state) %>%
  .$label

prior_party <- c('red4', 'purple2', 'blue4')

dat <- modeled_EV_state_spreads %>%
  select(-run_date) %>%
  rbind(modeled_EV_state_spreads_c2020) %>%
  left_join(dat_2020) %>%
  mutate(spread = ifelse(cycle == 2024, NA, spread))

dat_blue <- dat %>%
  filter(state != 'Arizona' & spread_mu < 0 | state == 'Maine CD-2') %>%
  mutate(party = case_when(spread_start > 0 & spread_end > 0 ~ 'rep',
                           spread_start < 0 & spread_end > 0 ~ 'toss',
                           spread_start < 0 & spread_end < 0 ~ 'dem'),
         party = factor(party, levels = c('rep', 'toss', 'dem')),
         label = paste(state, cycle),
         state = factor(state, vec_prior_ord),
         label = factor(label, label_ord))

prior_party <- c('purple2', 'blue4', 'red4')

dat_blue %>%
  ggplot(aes(x = label, y = spread_mu, ymin = spread_start, ymax = spread_end, color = party, grou = cycle, label = cycle)) +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = -0.1, ymax = 0.1, size = 0, fill = 'purple4', alpha = .2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -0.2, color = 'grey60', linetype = 2) +
  geom_hline(yintercept = c(-.1, .1), color = 'grey60', linetype = 2) +
  geom_errorbar(position = 'dodge') +
  geom_point(size = .5) +
  geom_point(aes(y = spread), shape = 23, size = 2, fill = 'yellow', color = 'black') +
  scale_y_continuous(breaks = seq(-1, 1, .05)) +
  geom_text(aes(y = ifelse(state == 'District of Columbia', spread_end, spread_start)),
            hjust = ifelse(dat_blue$state == 'District of Columbia', 0, 1),
            nudge_y = ifelse(dat_blue$state == 'District of Columbia', .02, -.02),
            size = 2) +
  scale_color_manual(values = prior_party) +
  coord_flip() +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90,
                                   size = rel(2)),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle('2020 Forecasted Spreads, Outcome, and Effect on 2024 for states that leaned Democrat in 2020') +
  labs(caption = 'Created by Andrew F. Griffin') +
  facet_grid(state ~ ., scales = 'free') +
  theme(strip.text.y = element_text(angle = 0),
        panel.border = element_rect(color = 'grey60', fill = NA, size = .5))

p_width <- 9
p_height <- (9/16) * p_width 

ggsave("figs/blue-state-2024.png",
       width = p_width,
       dpi = "retina")


# Visualize Forecasted Spread Changes - 2024 REPUBLICAN ------------------------
dat_red <- dat %>%
  filter(state != 'Maine CD-2' & spread_mu > 0 | state == 'Arizona') %>%
  mutate(party = case_when(spread_start > 0 & spread_end > 0 ~ 'rep',
                           spread_start < 0 & spread_end > 0 ~ 'toss',
                           spread_start < 0 & spread_end < 0 ~ 'dem'),
         party = factor(party, levels = c('rep', 'toss', 'dem')),
         label = paste(state, cycle),
         state = factor(state, vec_prior_ord),
         label = factor(label, label_ord))

prior_party <- c('red4', 'purple2', 'blue4')

dat_red %>%
  ggplot(aes(x = label, y = spread_mu, ymin = spread_start, ymax = spread_end, color = party, grou = cycle, label = cycle)) +
  annotate('rect', xmin = -Inf, xmax = Inf, ymin = -0.1, ymax = 0.1, size = 0, fill = 'purple4', alpha = .2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = .2, color = 'grey60', linetype = 2) +
  geom_hline(yintercept = c(-.1, .1), color = 'grey60', linetype = 2) +
  geom_errorbar(position = 'dodge') +
  geom_point(size = .5) +
  geom_point(aes(y = spread), shape = 23, size = 2, fill = 'yellow', color = 'black') +
  scale_y_continuous(breaks = seq(-1, 1, .05)) +
  geom_text(aes(y = ifelse(state == 'Nebraska CD-3', spread_start, spread_end)),
            hjust = ifelse(dat_red$state == 'Nebraska CD-3', 1, 0),
            nudge_y = ifelse(dat_red$state == 'Nebraska CD-3', -.02, .02),
            size = 2) +
  scale_color_manual(values = prior_party) +
  coord_flip() +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90,
                                   size = rel(2)),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle('2020 Forecasted Spreads, Outcome, and Effect on 2024 for states that leaned Republican in 2020') +
  labs(caption = 'Created by Andrew F. Griffin') +
  facet_grid(state ~ ., scales = 'free') +
  theme(strip.text.y = element_text(angle = 0),
        panel.border = element_rect(color = 'grey60', fill = NA, size = .5))

ggsave("figs/red-state-2024.png",
       width = p_width,
       dpi = "retina")


