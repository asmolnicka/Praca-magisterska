
library(tidyverse)

mins5 = c('[0,5)',   '[5,10)',  '[10,15)', '[15,20)', '[20,25)', '[25,30)', '[30,35)', '[35,40)', '[40,45)', '[45,45+)',
          '[45,50)', '[50,55)', '[55,60)', '[60,65)', '[65,70)', '[70,75)', '[75,80)', '[80,85)', '[85,90)', '[90,90+)')
colors_vec = c('hotpink', 'darkgoldenrod1', 'green3', 'brown1', 'steelblue3')



### Wykres liczby zmian w ciągu meczu (ogółem i kiedy o których numerach) ###

ggplot(subst) +                                                                          # histogram - minuty zmian
  geom_histogram(aes(x = minute), stat = 'count', fill = 'dodgerblue2', colour = 'white') +
  geom_vline(xintercept = 0, linetype = 'dashed', lwd = 0.7) +
  geom_vline(xintercept = 45, linetype = 'dashed', lwd = 0.7) +
  geom_vline(xintercept = 90, linetype = 'dashed', lwd = 0.7) +
  coord_cartesian(ylim = c(0, 100), xlim = c(1, 100)) +
  scale_x_continuous(breaks = seq(0, 105, 5), labels = seq(0, 105, 5))

ggplot(subst) +                                                                          # histogram - minuty zmian ~ nr zmiany
  geom_histogram(aes(x = minute, fill = which_sub), colour = 'white', stat = 'count', alpha = 0.8) +
  geom_density(aes(x = minute, fill = which_sub, y = after_stat(count)), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed', lwd = 0.7) +
  geom_vline(xintercept = 45, linetype = 'dashed', lwd = 0.7) +
  geom_vline(xintercept = 90, linetype = 'dashed', lwd = 0.7) +
  coord_cartesian(ylim = c(0, 100), xlim = c(1, 100)) +
  scale_x_continuous(breaks = seq(0, 105, 5), labels = seq(0, 105, 5)) +
  scale_fill_manual(name = 'which_sub', values = colors_vec)

ggplot(subst) +
  geom_bar(aes(x = wh_5_min, fill = which_sub), stat = 'count', colour = 'white') +      # słupkowy skumulowany - 5-minutówki zmian ~ nr zmiany
  scale_x_continuous(breaks = 1:20, labels = mins5) +
  coord_cartesian(xlim = c(1, 20)) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 1) +
  scale_fill_manual(name = 'which_sub', values = colors_vec)

ggplot(subst) +
  geom_bar(aes(x = wh_5_min, fill = which_sub), stat = 'count', colour = 'white',
           position = position_dodge2(width = 0.8, preserve = 'single')) +
  scale_x_continuous(breaks = 1:20, labels = mins5) +                                    # słupkowy grupowany - 5-minutówki zmian ~ nr zmiany
  coord_cartesian(ylim = c(0, 75), xlim = c(1, 20)) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 1) +
  scale_fill_manual(name = 'which_sub', values = colors_vec) +
  scale_colour_manual(name = 'which_sub', values = colors_vec)

ggplot(subst) +
  geom_bar(aes(x = wh_5_min, fill = substitution.reason), stat = 'count',                # słupkowy skumulowany - 5-minutówki zmian ~ powód zmiany
                                                                colour = 'white') +  
  scale_x_continuous(breaks = 1:20, labels = mins5) +
  coord_cartesian(xlim = c(1, 20)) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 1) +
  scale_fill_manual(name = 'Reason', values = c('green3', 'dodgerblue'))

ggplot(subst %>% filter(substitution.reason == 'Tactical')) +
  geom_bar(aes(x = wh_5_min, fill = which_sub), stat = 'count', colour = 'white') +      # słupkowy grupowany - 5-minutówki zmian ~ nr zmiany dla zmian taktycznych
  scale_x_continuous(breaks = 1:20, labels = mins5) +
  coord_cartesian(xlim = c(1, 20)) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 1) +
  scale_fill_manual(name = 'which_sub', values = colors_vec)

ggplot(subst %>% filter(substitution.reason == 'Injury')) +
  geom_bar(aes(x = wh_5_min, fill = which_sub), stat = 'count', colour = 'white') +      # słupkowy grupowany - 5-minutówki zmian ~ nr zmiany dla zmian z powodu kontuzji
  scale_x_continuous(breaks = 1:20, labels = mins5) +
  coord_cartesian(ylim = c(0, 10), xlim = c(1, 20)) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 1) +
  scale_fill_manual(name = 'which_sub', values = colors_vec)

ggplot(subst %>% filter(substitution.reason == 'Injury')) +
  geom_bar(aes(x = wh_5_min, fill = which_sub), stat = 'count', colour = 'white',        # słupkowy skumulowany - 5-minutówki zmian ~ nr zmiany dla zmian z powodu kontuzji
           position = position_dodge2(width = 0.8, preserve = 'single')) +      
  scale_x_continuous(breaks = 1:20, labels = mins5) +
  coord_cartesian(ylim = c(0, 10), xlim = c(1, 20)) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 1) +
  scale_fill_manual(name = 'which_sub', values = colors_vec)

ggpubr::ggarrange(                                                                       # słupkowy - 5-minutówki zmian, panel względem powodu zmian 
  ggplot(subst) +
    geom_bar(aes(x = wh_5_min), stat = 'count', fill = 'dodgerblue', colour = 'white') +
    scale_x_continuous(breaks = 1:20, labels = mins5) +
    coord_cartesian(ylim = c(0, 180), xlim = c(1, 20)) +
    geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 1) +
    geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 1) +
    geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 1) +
    labs(title = 'Wszystkie zmiany') +
    theme(plot.title = element_text(hjust = 0.5)),
  
  ggplot(subst %>% filter(substitution.reason == 'Tactical')) +
    geom_bar(aes(x = wh_5_min), stat = 'count', fill = 'dodgerblue', colour = 'white') +
    scale_x_continuous(breaks = 1:20, labels = mins5) +
    coord_cartesian(ylim = c(0, 180), xlim = c(1, 20)) +
    geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 1) +
    geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 1) +
    geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 1) +
    labs(title = 'Zmiany taktyczne') +
    theme(plot.title = element_text(hjust = 0.5)),
  
  ggplot(subst %>% filter(substitution.reason == 'Injury')) +
    geom_bar(aes(x = wh_5_min), stat = 'count', fill = 'dodgerblue', colour = 'white') +
    scale_x_continuous(breaks = 1:20, labels = mins5) +
    coord_cartesian(ylim = c(0, 180), xlim = c(1, 20)) +
    geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 1) +
    geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 1) +
    geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 1) +
    labs(title = 'Zmiany z powodu kontuzji') +
    theme(plot.title = element_text(hjust = 0.5)),
  ncol = 3, nrow = 1)

ggsave('zmiany.png', last_plot(), dpi = 500, height = 5, width = 30, units = 'in')



### Wykres liczby wykorzystanych zmian w meczu ###

ggplot(n_sub_1m_1t) +                                                                        # słupkowy - liczba zmian jednej drużyny w meczu
  geom_bar(aes(x = n_subs), stat = 'count', fill = 'dodgerblue', colour = 'white')


#n_sub_1m_1t =
subst %>%                                                                                    # liczba zmian jednej drużyny w meczu
  group_by(match_id, team_id) %>%
  summarise(n_subs = n()) #%>% ungroup() %>% select(n_subs) %>% table

"# to samo na podstawie ESA_lineups:

ESA_lineups %>%
  filter_at(vars(starts_with('positions.end_reason')),
            any_vars(stringr::str_detect(., 'Substitution - Off'))) %>%
  group_by(match_id, team_id) %>%
  summarise(n_subs = n())

 # to samo na podstawie ESA_events:   (ale bez użycia tabeli subst)

ESA_events_clean %>%
  filter(type.name == 'Substitution') %>%
  group_by(match_id, team.id) %>%
  summarise(n_subs = n())
"


### Statystyki minut, w których były zmiany o kolejnych numerach ###

subst %>%
  group_by(which_sub) %>%
  summarise(mean = mean(minute),
            median = median(minute),
            mode = which.max(tabulate(minute)),
            sd = sd(minute),
            n = n())



### Wykres okien, w których dokonywane były zmiany w ciągu meczu ###

ggplot(subst) +                                                                                       # słupkowy - liczba okien zmian jednej drużyny w meczu
  geom_bar(aes(x = which_sub_time), stat = 'count', fill = 'dodgerblue', colour = 'white')

ggplot(subst %>% filter()) +                                                                          # histogram - minuty zmian ~ nr okna zmian
  geom_histogram(aes(x = minute, fill = which_sub_time), colour = 'white', stat = 'count', alpha = 0.8) +
  geom_density(aes(x = minute, fill = which_sub_time, y = after_stat(count)), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed', lwd = 0.7) +
  geom_vline(xintercept = 45, linetype = 'dashed', lwd = 0.7) +
  geom_vline(xintercept = 90, linetype = 'dashed', lwd = 0.7) +
  coord_cartesian(ylim = c(0, 90), xlim = c(1, 100)) +
  scale_x_continuous(breaks = seq(0, 105, 5), labels = seq(0, 105, 5)) +
  scale_fill_manual(name = 'which_sub_time', values = colors_vec)

ggplot(subst) +
  geom_bar(aes(x = wh_5_min, fill = which_sub_time), stat = 'count', colour = 'white') +              # słupkowy skumulowany - 5-minutówki zmian ~ nr okna zmian 
  scale_x_continuous(breaks = 1:20, labels = mins5) +
  coord_cartesian(xlim = c(1, 20)) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 1) +
  scale_fill_manual(name = 'which_sub_time', values = colors_vec)

ggplot(subst) +                                                                                       # słupkowy grupowany - 5-minutówki zmian ~ nr okna zmian
  geom_bar(aes(x = wh_5_min, fill = which_sub_time), stat = 'count', colour = 'white',
           position = position_dodge2(width = 1, preserve = 'single')) +
  scale_x_continuous(breaks = 1:20, labels = mins5) +
  coord_cartesian(xlim = c(1, 20)) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 10.5, linetype = 'dashed', lwd = 1) +
  geom_vline(xintercept = 20.5, linetype = 'dashed', lwd = 1) +
  scale_fill_manual(name = 'which_sub_time', values = colors_vec) +
  scale_colour_manual(name = 'which_sub_time', values = colors_vec)



### Wykresy rozkładu zmian w stosunku do okien, w których się one odbywały ###

ggplot(subst %>% filter(which_sub != 1)) +                                                            # słupkowy (panel wykresów) - nr okna zmian ~ nr zmiany
  geom_bar(aes(x = which_sub_time), stat = 'count', fill = 'dodgerblue', colour = 'white') +
  scale_x_discrete(breaks = 1:3, labels = 1:3) +
  facet_grid(~which_sub, labeller = 'label_both')

ggplot(subst) +                                                                                       # słupkowy (panel wykresów) - nr zmiany ~ nr okna zmian
  geom_bar(aes(x = which_sub), stat = 'count', fill = 'dodgerblue', colour = 'white') +
  scale_x_discrete(breaks = 1:5, labels = 1:5) +
  facet_grid(~which_sub_time, labeller = 'label_both')

ggplot(subst %>%                                                                                      # słupkowy (panel wykresów) - liczba zmienionych zawodników ~ nr okna zmian
        group_by(match_id, team_id, which_sub_time) %>%
        summarise(n = n())) +
  geom_bar(aes(x = n), stat = 'count', fill = 'dodgerblue', colour = 'white') +
  facet_grid(~which_sub_time, labeller = 'label_both')

ggplot(subst %>%                                                                                      # słupkowy (panel wykresów) -
        group_by(match_id, team_id, which_sub_time) %>%                                               #  układ liczby zmienionych zawodników w każdym oknie
        summarise(n = n()) %>%
        ungroup() %>%
        group_by(match_id, team_id) %>%
        summarise(nsub = sum(n),
                  n_players_sub_times = stringr::str_c(n, collapse = '')) %>%
        arrange(n_players_sub_times)) +
  geom_bar(aes(x = n_players_sub_times), stat = 'count', fill = 'dodgerblue', colour = 'white',
           position = position_dodge2(width = 1, preserve = 'single')) +
  facet_grid(~nsub, labeller = 'label_both', scales = 'free_x', space = 'free')
      


### Wykres różnic między dokonywanymi zmianami (tylko w drugiej połowie) ###

subst %>%                                                                                             # słupkowy - różnice w minutach między zmianami
  filter(period == 2) %>%                                                                             #  tylko w drugiej połowie
  group_by(match_id, team_id) %>%
  summarise(diff_subs_in_mins = diff(second_total) / 60) %>%
  ggplot() +
  geom_histogram(aes(x = diff_subs_in_mins), fill = 'dodgerblue', colour = 'white', binwidth = 1) +
  coord_cartesian(xlim = c(0, 45)) +
  scale_x_continuous(breaks = seq(0, 45, 5), labels = seq(0, 45, 5))

subst %>%                                                                                             # słupkowy - różnice w minutach między zmianami, bez 0-wej minuty
  filter(period == 2) %>%                                                                             #  tylko w drugiej połowie
  group_by(match_id, team_id) %>%
  summarise(diff_subs_in_mins = diff(second_total) / 60) %>%
  ggplot() +
  geom_histogram(aes(x = diff_subs_in_mins), fill = 'dodgerblue', colour = 'white', binwidth = 1) +
  coord_cartesian(xlim = c(0, 45), ylim = c(0, 60)) +
  scale_x_continuous(breaks = seq(0, 45, 5), labels = seq(0, 45, 5))

subst %>%                                                                                             # procent zmian w każdej z połów                                      
  group_by(period) %>% 
  summarise(n_subs = n()/nrow(.))




### Zapisanie utworzonych obiektów ###

save(mins5, colors_vec,
     file = 'to_plot_N.RData')
load('to_plot_N.RData')



### Usunięcie zbędnych zmiennych ###

rm(mins5, colors_vec)

