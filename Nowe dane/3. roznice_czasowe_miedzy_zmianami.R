
library(tidyverse)



### Poglądowe wykresy, jaki punkt powinien być uznany za to samo lub nowe "okno" zmiany ###


ggplot(subst) +                                                                              # histogram - różnica sekund między zmianami 
  geom_bar(aes(x = secs_total_diff), fill = 'dodgerblue')
ggplot(subst %>% filter(secs_total_diff < 240)) +                                            # histogram - różnica sekund między zmianami (w ewentualnym jednym oknie)
  geom_bar(aes(x = secs_total_diff), fill = 'dodgerblue')
ggplot(subst %>% filter(secs_total_diff < 70)) +
  geom_bar(aes(x = secs_total_diff), fill = 'dodgerblue', colour = 'white') +
  scale_x_continuous(breaks = seq(0, 70, 5), labels = seq(0, 70, 5))

ggplot(subst) +                                                                              # histogram - różnica numerów eventów między zmianami 
  geom_bar(aes(x = event_ids_diff), fill = 'dodgerblue')
ggplot(subst %>% filter(event_ids_diff < 100)) +                                             
  geom_bar(aes(x = event_ids_diff), fill = 'dodgerblue') +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 300, 20))
ggplot(subst %>% filter(event_ids_diff < 30)) +                                              # histogram - różnica numerów eventów między zmianami (w ewentualnym jednym oknie) 
  geom_bar(aes(x = event_ids_diff), fill = 'dodgerblue') +
  scale_x_continuous(breaks = 0:30) +
  scale_y_continuous(breaks = seq(0, 300, 20))
ggplot(subst %>% filter(event_ids_diff < 10)) +
  geom_bar(aes(x = event_ids_diff), fill = 'dodgerblue') +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(breaks = seq(0, 300, 20))

ggplot(subst) +                                                                              # wykres rozrzutu jednych różnic do drugich
  geom_point(aes(x = secs_total_diff, y = event_ids_diff),
             color = 'dodgerblue2', alpha = 0.2)
ggExtra::ggMarginal(ggplot(subst %>% filter(secs_total_diff <= 210)) +                       # wykres rozrzutu jednych różnic do drugich (w ewentualnym jednym oknie) 
                      geom_point(aes(x = secs_total_diff, y = event_ids_diff),
                                 color = 'dodgerblue2', alpha = 0.2),
                    type = 'histogram', fill = 'dodgerblue2', col = 'white') 
ggExtra::ggMarginal(ggplot(subst %>% filter(secs_total_diff <= 65)) +                         
                      geom_point(aes(x = secs_total_diff, y = event_ids_diff),
                                 color = 'dodgerblue2', alpha = 0.2) +
                      scale_y_continuous(breaks = 1:9),
                    type = 'histogram', fill = 'dodgerblue2', col = 'white')  
ggExtra::ggMarginal(ggplot(subst %>% filter(secs_total_diff <= 65)) +
                      geom_point(aes(x = secs_total_diff, y = event_ids_diff),
                                 color = 'dodgerblue2', alpha = 0.2) +
                      scale_y_continuous(breaks = 1:9),
                    type = 'boxplot')  

ggplot(subst %>%                                                                             # wykres słupkowy - maksymalna różnica w sekundach dla różnicy w id eventów do 10
         filter(event_ids_diff < 10) %>%
         #filter(!(secs_total_diff > 65 & secs_total_diff <= 150)) %>%
         group_by(event_ids_diff) %>%
         summarise(max_sec_diff = max(secs_total_diff)),
       aes(x = event_ids_diff, y = max_sec_diff)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue2', colour = 'white') + 
  geom_text(aes(label = max_sec_diff), vjust = -0.2) +
  scale_x_continuous(breaks = 1:9) + 
  scale_y_continuous(breaks = seq(0, 140, 20))


ggplot(                                                                                      # wykres słupkowy - najczęstsze wydarzenia przed zmianą
  semi_join(ESA_events_clean,
            ESA_events_clean %>%                                                                                      
              filter(type.name == 'Substitution') %>%
              mutate(event_id = event_id - 1),
            by = c('match_id', 'event_id')) %>%
    group_by(type.name) %>%
    summarise(n = n()) %>%
    arrange(-n) %>%
    mutate(type.name = factor(type.name, levels = .$type.name)) %>%
    slice(1:7),
  aes(x = type.name, y = n)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue2', colour = 'white')

ggplot(                                                                                      # wykres słupkowy - najczęstsze wydarzenia po zmianie                          
  semi_join(ESA_events_clean,
            ESA_events_clean %>%                                                                                      
              filter(type.name == 'Substitution') %>%
              select(match_id, event_id) %>%
              mutate(event_id = event_id + 1),
            by = c('match_id', 'event_id')) %>%
    group_by(type.name) %>%
    summarise(n = n()) %>%
    arrange(-n) %>%
    mutate(type.name = factor(type.name, levels = .$type.name)) %>%
    slice(1:7),
  aes(x = type.name, y = n)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue2', colour = 'white')  



get_nearby_events = function(df_me, k){
  colnam = c('match_id', 'team_id', 'team.name', 'event_id', 'period', 'timestamp',                    # funkcja wybierająca fragmenty eventów na podstawie          
             'second_total_in_per', 'type.name', 'possession_team.name', 'play_pattern.name')          #   podanej ramki danych z match_id i event_id
  
  rlist::list.rbind(lapply(1:nrow(df_me), function(i)
    rbind(
      ESA_events_clean %>%
        mutate(second_total_in_per = as.integer(stringr::str_sub(timestamp, 4, 5)) * 60 +                       
                 as.integer(stringr::str_sub(timestamp, 7, 8))) %>%
        filter(match_id == df_me$match_id[i] &
                 event_id %in% (df_me$event_id - k)[i]:(df_me$event_id + k)[i]) %>%
        select(all_of(colnam)),
      if (i != nrow(df_me)) rep(NA, length(colnam)))))
}


ev_ids_for_nearby_subs =
  subst %>%                                                                                            # różnica w eventach dla drugiej najmniejszej różnicy
    group_by(match_id, team_id) %>%                                                                    #  w sekundach (czyli po tej, która stworzyła okno)
    filter(length(unique(which_sub_time)) > 3) %>%                                                     #  dla zmian 4-oknowych (sprawdzenie czy nie powinno być jeszcze jedno okno)
    filter(is.na(secs_total_diff) | secs_total_diff != min(secs_total_diff, na.rm = TRUE)) %>%
    filter(secs_total_diff == min(secs_total_diff, na.rm = TRUE)) %>%
    filter(event_ids_diff < 10) %>%                                                                    #  tylko z liczbą eventów w eventualnym oknie zmiany (do 10)
    ungroup() %>%
    #select(secs_total_diff, event_ids_diff) %>% arrange(secs_total_diff) #%>% print(n = 44)
    select(match_id, event_id)

View(get_nearby_events(ev_ids_for_nearby_subs, k = 6))                                                 # podgląd eventów w pobliżu znalezionych zmian

View(get_nearby_events(subst %>%                                                                       # podgląd eventów w pobliżu zmian w odstępie 40-65s
                         filter(secs_total_diff >= 40 & secs_total_diff <= 65 & event_ids_diff > 1) %>%
                         select(match_id, event_id), k = 6))

View(get_nearby_events(subst %>%                                                                       # podgląd eventów w pobliżu zmian w odstępie 2-10 eventów
                         filter(event_ids_diff > 1 & event_ids_diff < 10 & secs_total_diff < 40) %>%
                         select(match_id, event_id), k = 6))

View(get_nearby_events(subst %>%                                                                       # podgląd eventów w pobliżu zmian w odstępie 65-150s
                         filter(secs_total_diff > 65 & secs_total_diff <= 150) %>%
                         select(match_id, event_id), k = 10))

View(get_nearby_events(subst %>%                                                                       # podgląd eventów w pobliżu zmian w odstępie 65-150s i 2-10 eventów
                         filter(secs_total_diff > 65 & secs_total_diff <= 150 & event_ids_diff < 10) %>%
                         select(match_id, event_id), k = 10))

# Eventy pomiędzy zmianami w prawdopodobnych oknach:
#  Pass, Ball Recovery, Carry, Tactical Shift, Player Off, Bad Behaviour, Injury Stoppage (?), Clearence (?), Substitution (?)


# Chyba najlepszy pomysł na granicę: Gdy nie było kontuzji okno do 75s, a gdy była do 150s. Wśród eventów pomiędzy zmianami były tylko powyższe.
# Taka implementacja jest w pliku różne.










