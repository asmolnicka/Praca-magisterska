
library(tidyverse)

colors_vec = c('hotpink', 'darkgoldenrod1', 'green3', 'brown1', 'steelblue3')



### Zbiór zmian zawodników z trenerami ich drużyn ###

subst_manager = merge(subst, ESA_Matches %>%
                        select(match_id,
                               home_team.home_team_id,
                               home_team.managers.id,
                               away_team.away_team_id,
                               away_team.managers.id), by = 'match_id') %>%
  tibble::add_column(team.manager.id = if_else(.$team_id == .$home_team.home_team_id,
                                               .$home_team.managers.id,
                                               .$away_team.managers.id),
                     .after = 'which_team') %>%
  select(-c(home_team.home_team_id,
            home_team.managers.id,
            away_team.away_team_id,
            away_team.managers.id)) %>%
  filter(!is.na(team.manager.id)) %>% as_tibble()


### Wykres liczby zmian (taktycznych) w ciągu meczu (w 5-minutówkach) (ogółem i kiedy o których numerach) względem danego trenera ###

ggplot(subst_manager %>% filter(substitution.reason == 'Tactical')) +                           # ogółem
  geom_bar(aes(x = wh_5_min), stat = 'count', colour = 'white', fill = 'dodgerblue3', alpha = 0.7) +
  scale_x_continuous(breaks = 1:20, labels = c(seq(0,40,5), '45', seq(45,85,5), '90')) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 0.8, alpha = 0.5) +
  geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 0.8, alpha = 0.5) +
  geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 0.8, alpha = 0.5) +
  facet_wrap(~ team.manager.id, scales = 'free') +
  theme(axis.text.x = element_text(size = 6))

ggplot(subst_manager %>% filter(substitution.reason == 'Tactical')) +                           # kiedy która
  geom_bar(aes(x = wh_5_min, fill = which_sub), stat = 'count', colour = 'white', alpha = 0.7) +
  scale_x_continuous(breaks = 1:20, labels = c(seq(0,40,5), '45', seq(45,85,5), '90')) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 0.8, alpha = 0.5) +
  geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 0.8, alpha = 0.5) +
  geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 0.8, alpha = 0.5) +
  scale_fill_manual(name = 'which_sub', values = colors_vec) +
  facet_wrap(~ team.manager.id, scales = 'free') +
  theme(axis.text.x = element_text(size = 6))

ggplot(subst_manager %>% filter(substitution.reason == 'Tactical' & period == 2)) +             # ogółem, 2. połowa
  geom_bar(aes(x = wh_5_min), stat = 'count', colour = 'white', fill = 'dodgerblue3', alpha = 0.7) +
  scale_x_continuous(breaks = 11:20, labels = c(seq(45,85,5), '90')) +
  coord_cartesian(xlim = c(11, 20)) +
  facet_wrap(~ team.manager.id, scales = 'free') +
  theme(axis.text.x = element_text(size = 9))

ggplot(subst_manager %>% filter(substitution.reason == 'Tactical' & period == 2)) +             # kiedy która, 2. połowa
  geom_bar(aes(x = wh_5_min, fill = which_sub), stat = 'count', colour = 'white', alpha = 0.7) +
  scale_x_continuous(breaks = 11:20, labels = c(seq(45,85,5), '90')) +
  coord_cartesian(xlim = c(11, 20)) +
  scale_fill_manual(name = 'which_sub', values = colors_vec) +
  facet_wrap(~ team.manager.id, scales = 'free') +
  theme(axis.text.x = element_text(size = 9))


### Wykres liczby zmian robionych wielokrotnie przez danego trenera ###

subst_manager %>%                                                                                     # najczęściej dokonywane zmiany ~ trener
        group_by(team.manager.id, player_id, player.on.id) %>%
        summarise(n_the_same_sub = n()) %>%
        arrange(-n_the_same_sub)

ggplot(subst_manager %>%                                                                              # histogram - najczęściej dokonywane zmiany ~ trener
         group_by(team.manager.id, player_id, player.on.id) %>%
         summarise(n_the_same_sub = n())) +
  geom_bar(aes(x = n_the_same_sub), stat = 'count', colour = 'white', fill = 'dodgerblue3', alpha = 0.7) +
  coord_cartesian(xlim = c(1, 7)) +
  scale_x_continuous(breaks = 1:7) + 
  facet_wrap(~team.manager.id, scales = 'free')



### Zapis utworzonych tabel ###

save(subst_manager, colors_vec,
     file = 'subst_manager_table_N.RData')
load('subst_manager_table_N.RData')














