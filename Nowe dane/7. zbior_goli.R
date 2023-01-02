
library(tidyverse)



### zbiór goli ###

goals = ESA_events_clean %>%
  filter(shot.outcome.name == 'Goal') %>%
  select(c(match_id, event_id, 
           period, timestamp, minute, second, second_total_in_per, second_total,
           team_id, player_id))

goals = merge(goals, 
              ESA_Matches %>%
                select(match_id,
                       home_team.home_team_id,
                       away_team.away_team_id), by = 'match_id') %>%
                tibble::add_column(which_team = if_else(.$team_id == .$home_team.home_team_id,
                                                        'home_team',
                                                        'away_team'),
                                   .after = 'team_id') %>%
                select(-c(home_team.home_team_id,
                          away_team.away_team_id)) %>%
                arrange(match_id, event_id) %>% as_tibble()

goals = goals %>%
  group_by(match_id) %>%
  mutate(home_team_goal = cumsum(which_team == 'home_team'),
         away_team_goal = cumsum(which_team == 'away_team'),
         goals_diff = home_team_goal - away_team_goal) %>%
  ungroup() %>%
  tibble::add_column(which_team_goal = if_else(.$which_team == 'home_team', .$home_team_goal, .$away_team_goal),
                                                       .after = 'player_id')



### zbiór zmian i goli ###

subst_goals = data.table::rbindlist(list(sub = subst %>%
                                                  select(c(match_id, event_id, 
                                                           period, timestamp, minute, second, second_total_in_per, second_total,
                                                           team_id, which_team, which_sub)),
                                         goal = goals %>%
                                                  select(-player_id)),
                                    fill = TRUE, idcol = 'event.type') %>%
  relocate(event.type, .after = second_total) %>%
  tidyr::unite('which_team_the.event', 'which_sub':'which_team_goal', sep = '', na.rm = TRUE) %>%
  arrange(match_id, event_id) %>%
  fill(home_team_goal:goals_diff, .direction = 'down') %>%
  tidyr::replace_na(list(home_team_goal = 0, away_team_goal = 0, goals_diff = 0)) %>% as_tibble()



### Wykres zmian w zależności od wyniku meczu ###

subst_goals_plot = function(x){
  max_second_total = max(ESA_events_clean %>%
                           filter(match_id == x) %>%
                           select(second_total)) 
  goals_data = (goals %>%
                        filter(match_id == x) %>%
                        select(second_total, goals_diff))
  goals_data_ext = rbind(c(0, 0),
                         goals_data,
                         c(max_second_total, goals_data$goals_diff[nrow(goals_data)]))
  subst_data = (subst_goals %>%
                  filter(match_id == x) %>%
                  filter(event.type == 'sub') %>%
                  select(which_team, second_total, goals_diff))  
  ggplot(goals_data_ext, aes(x = second_total, y = goals_diff)) + 
    geom_point() +
    geom_step() +
    emoGG::geom_emoji(data = goals_data, emoji = '26bd', size = 0.04) +
    geom_point(data = subst_data, aes(x = second_total, y = goals_diff, colour = which_team),
               size = 3, alpha = 0.4) +
    coord_cartesian(xlim = c(0, max_second_total), ylim = c(-3, 3)) + 
    geom_hline(yintercept = 0, alpha = 0.4, col = 'red')
}

subst_goals_plot(3799869)

subst_goals_plot = function(x){
  max_goals_diff = max(abs(goals %>%
                             filter(match_id == x) %>%
                             select(goals_diff)))
  max_second_total = max(ESA_events_clean %>%
                           filter(match_id == x) %>%
                           select(second_total)) 
  goals_data = (goals %>%
                        filter(match_id == x) %>%
                        select(which_team, second_total, goals_diff))
  goals_data_ext = rbind(c(0, 0),
                         goals_data %>% select(second_total:goals_diff),
                         c(max_second_total, goals_data$goals_diff[nrow(goals_data)]))
  subst_data = (subst_goals %>%
                  filter(match_id == x) %>%
                  filter(event.type == 'sub') %>%
                  select(which_team, second_total, goals_diff))  
  ggplot(goals_data_ext, aes(x = second_total, y = goals_diff)) + 
    geom_point() +
    geom_step() +
    geom_segment(data = subst_data, aes(x = second_total, xend = second_total, y = goals_diff,
                                        yend = if_else(which_team == 'home_team', max_goals_diff + 0.5, -(max_goals_diff + 0.5))),
                 alpha = 0.4, col = 'dodgerblue3', linetype = 'dashed') +
    emoGG::geom_emoji(data = subst_data,
                      aes(x = second_total,
                          y = if_else(which_team == 'home_team', max_goals_diff + 0.5, -(max_goals_diff + 0.5))),
                      emoji = '1f501', size = 0.04) +
    emoGG::geom_emoji(data = goals_data, 
                      aes(x = second_total,
                          y = if_else(which_team == 'home_team', max_goals_diff + 0.5, -(max_goals_diff + 0.5))),
                      emoji = '26bd', size = 0.04) +
    geom_hline(yintercept = 0, alpha = 0.4, col = 'red') +
    geom_vline(xintercept = 0, alpha = 0.4, col = 'forestgreen') +
    geom_vline(xintercept = 45 * 60, alpha = 0.4, col = 'forestgreen') +
    geom_vline(xintercept = 45 * 60 + (period_length %>% filter(match_id == x, period == 1) %>% select(add_time_length) %>% unlist),
               alpha = 0.4, col = 'forestgreen') +    
    geom_vline(xintercept = max_second_total, alpha = 0.4, col = 'forestgreen') +
    coord_cartesian(xlim = c(0, max_second_total), ylim = c(-(max_goals_diff + 1), max_goals_diff + 1)) + 
    scale_x_continuous(name = 'minute',
                       breaks = c(seq(0, 45 * 60, 5 * 60),
                                  seq(0, 45 * 60, 5 * 60) +
                                  (period_length %>% filter(match_id == x, period == 1) %>% select(max_second_total_in_per) %>% unlist),
                                  max_second_total),
                       labels = c(seq(0, 40, 5), '45+', seq(45, 85, 5), '90+', 'end')) +
    scale_y_continuous(name = 'goals diff', breaks = -(max_goals_diff + 1):(max_goals_diff + 1),
                       sec.axis = sec_axis(~ ., name = 'team', breaks = c(-(max_goals_diff + 0.5), max_goals_diff + 0.5),
                                           labels = c('away', 'home'))) +
    theme(legend.position = 'none',
          axis.text.y = element_text(size = 12))  
}

subst_goals_plot(3799869)
subst_goals_plot(3799867)



### Wykresy częstości zmian w zależności od wyniku meczu ###

ggplot(subst_goals %>%                                                                                # histogram - zmiana ~ różnica w golach
        filter(period == 2) %>%
        mutate(goals_diff = if_else(which_team == 'home_team', goals_diff, -goals_diff))) +
  geom_bar(aes(x = goals_diff), stat = 'count', fill = 'dodgerblue', colour = 'white') +
  scale_x_continuous(breaks = -5:5) 

ggplot(subst_goals %>%                                                                                # histogram - zmiana ~ różnica w golach ~ kwadrans 2. połowy
         filter(period == 2) %>%
         mutate(goals_diff = if_else(which_team == 'home_team', goals_diff, -goals_diff),
                which_quarter = case_when((minute < 60) ~ '45-60',
                                          (minute < 75) ~ '60-75',
                                          (minute < 90) ~ '75-90',
                                          TRUE ~ '90+'))) +
  geom_bar(aes(x = goals_diff), stat = 'count', fill = 'dodgerblue', colour = 'white') +
  scale_x_continuous(breaks = -5:5) +
  coord_cartesian(xlim = c(-5, 5)) +
  facet_wrap(~which_quarter, scales = 'free')



### Wykresy dotyczące odległości czasowej zmian od najbliższych goli w meczu ###

ggplot(subst_goals %>%                                                                                # histogram - minuta zmiany - minuta poprzedniego gola
         group_by(match_id) %>%
         mutate(goals_sum = home_team_goal + away_team_goal) %>%
         ungroup() %>%
         filter(period == 2) %>%
         group_by(match_id, goals_sum) %>%
         mutate(from.last.goal_minute = na_if(if_else(goals_sum != 0 & 'goal' %in% event.type, minute - min(minute), as.integer(-1)), as.integer(-1)),
                last_goal_team = na_if(if_else(goals_sum != 0 & 'goal' %in% event.type, which_team[which.min(minute)], 'no goal'), 'no goal'),
                own.opposite_team_goal = if_else(which_team == last_goal_team, 'own', 'opposite')) %>%
         filter(event.type == 'sub')) +
  geom_histogram(aes(x = from.last.goal_minute, fill = own.opposite_team_goal), colour = 'white') +
  scale_fill_manual(values = c('royalblue', 'darkorange2'))

ggplot(subst_goals %>%                                                                                # histogram - minuta zmiany - minuta kolejnego gola
         group_by(match_id) %>%
         mutate(goals_sum_rev = FSA::rcumsum(as.integer(event.type == 'goal'))) %>%
         ungroup() %>%
         filter(period == 2) %>%
         group_by(match_id, goals_sum_rev) %>%
         mutate(to.next.goal_minute = na_if(if_else(goals_sum_rev != 0 & 'goal' %in% event.type, max(minute) - minute, as.integer(-1)), as.integer(-1)),
                next_goal_team = na_if(if_else(goals_sum_rev != 0 & 'goal' %in% event.type, which_team[which.max(minute)], 'no goal'), 'no goal'),
                own.opposite_team_goal = if_else(which_team == next_goal_team, 'own', 'opposite')) %>%
         filter(event.type == 'sub')) +
  geom_histogram(aes(x = to.next.goal_minute, fill = own.opposite_team_goal), colour = 'white') +
  scale_fill_manual(values = c('royalblue', 'darkorange2'))

ggplot(subst_goals %>%                                                                                # histogram - minuta zmiany - minuta poprzedniego gola własnej drużyny
         filter(period == 2) %>%
         group_by(match_id, away_team_goal) %>%
         mutate(from.last.goal_minute_home = na_if(if_else(home_team_goal != 0 & 'goal' %in% event.type, minute - min(minute), as.integer(-1)), as.integer(-1)),
                from.last.goal_minute_away = na_if(if_else(away_team_goal != 0 & 'goal' %in% event.type, minute - min(minute), as.integer(-1)), as.integer(-1))) %>%
         filter(event.type == 'sub') %>%
         pivot_longer(names_to = 'which_team_sub',
                      cols = c('from.last.goal_minute_home', 'from.last.goal_minute_away'),
                      values_to = 'from.last.goal_minute')) +
  geom_histogram(aes(x = from.last.goal_minute), fill = 'dodgerblue', colour = 'white') 

ggplot(subst_goals %>%                                                                                # histogram - minuta zmiany - minuta kolejnego gola własnej drużyny
         group_by(match_id) %>%
         mutate(home_team_goal_rev = FSA::rcumsum(as.integer(event.type == 'goal' & which_team == 'home_team')),
                away_team_goal_rev = FSA::rcumsum(as.integer(event.type == 'goal' & which_team == 'away_team'))) %>%
         ungroup() %>%
         filter(period == 2) %>%
         group_by(match_id, away_team_goal_rev) %>%
         mutate(to.next.goal_minute_home = na_if(if_else(home_team_goal_rev != 0 & 'goal' %in% event.type, max(minute) - minute, as.integer(-1)), as.integer(-1)),
                to.next.goal_minute_away = na_if(if_else(away_team_goal_rev != 0 & 'goal' %in% event.type, max(minute) - minute, as.integer(-1)), as.integer(-1))) %>%
         filter(event.type == 'sub') %>%
         pivot_longer(names_to = 'which_team_sub',
                      cols = c('to.next.goal_minute_home', 'to.next.goal_minute_away'),
                      values_to = 'to.next.goal_minute')) +
  geom_histogram(aes(x = to.next.goal_minute), fill = 'dodgerblue', colour = 'white') 

ggplot(subst_goals %>%                                                                                # histogram - minuta zmiany - minuta poprzedniego gola (oba w drużynie home)
         filter(period == 2) %>%
         group_by(match_id, home_team_goal) %>%
         mutate(from.last.goal_minute = na_if(if_else(home_team_goal != 0 & 'goal' %in% event.type, minute - min(minute), as.integer(-1)), as.integer(-1))) %>%
         filter(event.type == 'sub')) +
  geom_histogram(aes(x = from.last.goal_minute), fill = 'dodgerblue', colour = 'white') 

ggplot(subst_goals %>%                                                                                # histogram - minuta zmiany - minuta kolejnego gola (oba w drużynie away)
         filter(period == 2) %>%
         group_by(match_id, away_team_goal) %>%
         mutate(from.last.goal_minute = na_if(if_else(away_team_goal != 0 & 'goal' %in% event.type, minute - min(minute), as.integer(-1)), as.integer(-1))) %>%
         filter(event.type == 'sub')) +
  geom_histogram(aes(x = from.last.goal_minute), fill = 'dodgerblue', colour = 'white')

ggplot(subst_goals %>%                                                                                # histogram - minuta zmiany - minuta kolejnego gola (oba w drużynie home)
         group_by(match_id) %>%
         mutate(home_team_goal_rev = FSA::rcumsum(as.integer(event.type == 'goal' & which_team == 'home_team'))) %>%
         ungroup() %>%
         filter(period == 2) %>%
         group_by(match_id, home_team_goal_rev) %>%
         mutate(to.next.goal_minute = na_if(if_else(home_team_goal_rev != 0 & 'goal' %in% event.type, max(minute) - minute, as.integer(-1)), as.integer(-1))) %>%
         filter(event.type == 'sub')) +
  geom_histogram(aes(x = to.next.goal_minute), fill = 'dodgerblue', colour = 'white') 

ggplot(subst_goals %>%                                                                                # histogram - minuta zmiany - minuta kolejnego gola (oba w drużynie away)
         group_by(match_id) %>%
         mutate(away_team_goal_rev = FSA::rcumsum(as.integer(event.type == 'goal' & which_team == 'away_team'))) %>%
         ungroup() %>%
         filter(period == 2) %>%
         group_by(match_id, away_team_goal_rev) %>%
         mutate(to.next.goal_minute = na_if(if_else(away_team_goal_rev != 0 & 'goal' %in% event.type, max(minute) - minute, as.integer(-1)), as.integer(-1))) %>%
         filter(event.type == 'sub')) +
  geom_histogram(aes(x = to.next.goal_minute), fill = 'dodgerblue', colour = 'white') 



### Zapis utworzonych tabel ###

save(goals, subst_goals, subst_goals_plot,
     file = 'subst_goals_table_N.RData')
load('subst_goals_table_N.RData')



### Usunięcie zbędnych zmiennych ###

rm()








