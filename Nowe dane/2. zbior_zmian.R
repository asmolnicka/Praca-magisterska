
library(tidyverse)



### Zbiór zmian zawodników ###

subst = ESA_events_clean %>%                                                                                     # tabela - zmiany pobrane z tabeli events
          filter(type.name == 'Substitution') %>%
          select(c(match_id, team_id, event_id, 
                   period, timestamp, minute, second, second_total_in_per, second_total,
                   player_id, substitution.replacement.id, substitution.outcome.name)) %>%
          rename(player.on.id = substitution.replacement.id,
                 substitution.reason = substitution.outcome.name) %>%
          mutate(wh_5_min = case_when((period == 1 & minute < 45) ~ floor(minute / 5) + 1,                            # kol. - która 5-minutówka meczu
                                      (period == 1 & minute >= 45) ~  10,
                                      (period == 2 & minute < 90) ~ floor(minute / 5) + 2,
                                      (period == 2 & minute >= 90) ~ 20)) %>%
          arrange(event_id)

n_sub_1m_1t = subst %>%                                                                                           # tabela - liczba wykorzystanych zmian danej drużyny w danym meczu
  group_by(match_id, team_id) %>%
  summarise(n_subs = n()) %>%
  arrange(match_id, team_id)

l.1m1t = rlist::list.rbind(lapply(1:nrow(n_sub_1m_1t), function(i) {                                              # tabela - nr zmiany oraz "okna" tej zmiany danej drużyny w danym meczu
  l.1m1t_init = subst %>%
    filter(match_id == n_sub_1m_1t$match_id[i] & team_id == n_sub_1m_1t$team_id[i]) %>%
    arrange(event_id) %>%
    mutate(event_ids_diff = c(c(if (1 %in% (.)$period) NA,                                                            # kol. - różnica w numerach eventów od ostatniej zmiany w danej połowie
                                diff((.) %>% filter(period == 1) %>% select(event_id) %>% unlist)),
                              c(if (2 %in% (.)$period) NA,
                                diff((.) %>% filter(period == 2) %>% select(event_id) %>% unlist))),
           secs_total_diff = c(c(if (1 %in% (.)$period) NA,                                                           # kol. - różnica w sekundach od ostatniej zmiany w danej połowie
                                 diff((.) %>% filter(period == 1) %>% select(second_total_in_per) %>% unlist)),
                               c(if (2 %in% (.)$period) NA,
                                 diff((.) %>% filter(period == 2) %>% select(second_total_in_per) %>% unlist))))
  l.1m1t_init %>%
    mutate(events_before_sub = lapply(1:nrow(.), function(j) (ESA_events_clean %>%                                    # kol. - eventy od ostatniej zmiany
                                                                filter(match_id == n_sub_1m_1t$match_id[i]) %>%           
                                                                filter(event_id > (l.1m1t_init$event_id[j] - l.1m1t_init$event_ids_diff[j]) &
                                                                       event_id < l.1m1t_init$event_id[j]))$type.name),
           which_sub = 1:nrow(.),                                                                                     # kol. - nr zmiany
           which_sub_time = sapply(1:nrow(.), function(j) { if (j == 1) {                                             # kol. - nr okna
                                                               nw <<- 1
                                                            } else {
                                                               if (is.na(event_ids_diff[j])) {
                                                                  nw <<- nw + 1
                                                               } else {
                                                                  nw <<- nw + (event_ids_diff[j] > 1 & secs_total_diff[j] > 75)}};
                                                            nw})) %>%
    select(c(match_id, event_id, which_sub, which_sub_time, secs_total_diff, event_ids_diff, events_before_sub))
}))

subst = merge(subst, l.1m1t, by = c('match_id', 'event_id'), sort = TRUE) %>% as_tibble %>%                       # JOIN głóWnej tabeli z powyższą, zmiana typów zmiennych oraz ich kolejności
              mutate(second_total = as.integer(second_total),
                     second_total_in_per = as.integer(second_total_in_per),
                     period = as.integer(period),
                     wh_5_min = as.integer(wh_5_min),
                     secs_total_diff = as.integer(secs_total_diff),
                     which_sub = as.factor(which_sub),
                     which_sub_time = as.factor(which_sub_time)) %>%
              relocate(c(second_total_in_per,
                         second_total,
                         wh_5_min,
                         which_sub,
                         which_sub_time,
                         secs_total_diff,
                         event_ids_diff), .after = second) %>%
              arrange(match_id, event_id)

subst = merge(subst,                                                                                              # JOIN tabeli z ESA_Matches dla kolumny z oznaczeniem home/away team
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

subst = subst %>%                                                                                                 # odfiltrowanie meczy z 4 oknami zmian
          filter(!(match_id %in% (subst %>%
                                    group_by(match_id, team_id) %>%                                                                    
                                    filter(length(unique(which_sub_time)) > 3) %>%
                                    ungroup() %>%
                                    distinct(match_id) %>%
                                    unlist())))



### Zapis utworzonych tabel ###

save(subst, n_sub_1m_1t,
     file = 'subst_table_N.RData')
load('subst_table_N.RData')



### Usunięcie zbędnych zmiennych ###

rm(nw, l.1m1t)



