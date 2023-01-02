
library(tidyverse)
library(StatsBombR)



### Pobranie danych ###

# dane do logowania

USER = 'user_mail'
PWD = 'password'

# get data competitions

comps = competitions(USER, PWD)                                        # które zawody/turniej i  w którym sezonie

# filter for the competitions you want

ESAComps = comps

# create a matrix of the competition and season ids

competitionmatrix = as.matrix(ESAComps[2, 1:2])                        # wybór turnieju w danym sezonie
ESA_Matches = MultiCompMatches(USER, PWD, competitionmatrix)           # pobranie właściwych danych (o meczach) dla danego turnieju

lineups = alllineups(USER, PWD, ESA_Matches$match_id, parallel = T)    # pobranie składów meczów
ESA_lineups = cleanlineups(lineups)                                    # rozwinięcie poziomów składów meczów

# pull all of the events

ESA_events = MultiCompEvents(USER, PWD, competitionmatrix)             # pobranie właściwych danych (o eventach) dla danego turnieju

# get all free events data from those games

## ESA_events = StatsBombFreeEvents(MatchesDF = ESA_Matches)


# apply clean functions from StatsbombR package to get parsed coordinates, possession info, etc.

ESA_events_clean = ESA_events %>% allclean()                           # czyszczenie danych z eventami



### Zapis danych po pobraniu ###

save(comps,
     ESAComps,
     competitionmatrix,
     ESA_Matches,
     lineups,
     ESA_lineups,
     ESA_events,
     ESA_events_clean,
file = 'data_begin_N.RData')
load('data_begin_N.RData')



### Podgląd danych ###

# ESAComps

knitr::kable(ESAComps) %>% kableExtra::kable_styling(font_size = 12)
dplyr::glimpse(ESAComps)      
skimr::skim(ESAComps)
summarytools::dfSummary(ESAComps, round.digits = 2) %>%
  summarytools::view(footnote = '', custom.css = 'tiny_text.css', table.classes = 'tiny-text')

# ESA_Matches

knitr::kable(ESA_Matches) %>% kableExtra::kable_styling(font_size = 12)
dplyr::glimpse(ESA_Matches)                             
skimr::skim(ESA_Matches)
summarytools::dfSummary(ESA_Matches, round.digits = 2) %>%
  summarytools::view(footnote = '', custom.css = 'tiny_text.css', table.classes = 'tiny-text')

# ESA_lineups

knitr::kable(ESA_lineups) %>% kableExtra::kable_styling(font_size = 12)
dplyr::glimpse(ESA_lineups)      
skimr::skim(ESA_lineups)
summarytools::dfSummary(ESA_lineups, round.digits = 2) %>%
  summarytools::view(footnote = '', custom.css = 'tiny_text.css', table.classes = 'tiny-text')

# ESA_events_clean

knitr::kable(ESA_events_clean) %>% kableExtra::kable_styling(font_size = 12)
dplyr::glimpse(ESA_events_clean)                             
skimr::skim(ESA_events_clean)
summarytools::dfSummary(ESA_events_clean, round.digits = 2) %>% summarytools::view(method = 'browser')



### Modyfikacja danych ###

# zmiana kolejności kolumn

ESAComps = ESAComps %>%
  select(c('season_id',
           'season_name',
           'competition_id',
           'competition_name',
           'country_name',
           'competition_international',
           'competition_gender',
           'competition_youth',
           'match_updated',
           'match_updated_360',
           'match_available',
           'match_available_360'))

ESA_lineups = ESA_lineups %>%
  select(c('player_id',
           'player_name',
           'player_nickname',
           'player_gender',
           'birth_date',
           'player_height',
           'player_weight',
           'country.id',
           'country.name',
           'team_id',
           'team_name',
           'jersey_number',
           'match_id',
           'formations',
           'positions',
           'stats',
           'events'))

ESA_Matches = ESA_Matches %>%
  select(c('match_id',
           'competition.competition_id',
           'competition.competition_name',
           'competition.country_name',
           'season.season_id',
           'season.season_name',
           'competition_stage.id',
           'competition_stage.name',    
           'match_week',
           'match_date',
           'kick_off',
           'stadium.id',
           'stadium.name',
           'stadium.country.id',     
           'stadium.country.name',
           'referee.id',
           'referee.name',
           'referee.country.id',
           'referee.country.name',
           'home_team.home_team_id',      
           'home_team.home_team_name',
           'home_team.home_team_gender',
           'home_team.home_team_youth',
           'home_team.home_team_group',
           'home_team.country.id',
           'home_team.country.name',
           'home_team.managers',
           'away_team.away_team_id',      
           'away_team.away_team_name',
           'away_team.away_team_gender',
           'away_team.away_team_youth',
           'away_team.away_team_group',
           'away_team.country.id',
           'away_team.country.name',
           'away_team.managers',
           'home_score',
           'away_score',
           'neutral_ground',
           'behind_closed_doors',
           'attendance',
           'play_status',
           'match_status',
           'match_status_360',
           'last_updated',
           'last_updated_360',
           'metadata.data_version',
           'metadata.shot_fidelity_version',
           'metadata.xy_fidelity_version'))

# rozwinięcie ESA_Matches (home_team.managers, away_team.managers)

managers_data = c('id', 'name', 'nickname', 'dob', 'country.id', 'country.name')

for (i in 1:nrow(ESA_Matches)){
  if (is.null(ESA_Matches$home_team.managers[[i]])){
    ESA_Matches$home_team.managers[[i]] = structure(data.frame(matrix(rep(NA, length(managers_data)), nrow = 1)),
                                                    names = managers_data)
  }
  nr = nrow(ESA_Matches$home_team.managers[[i]])
  if (nr > 1){
    ESA_Matches$home_team.managers[[i]] =
      ESA_Matches$home_team.managers[[i]] %>% unlist %>% data.frame(feat = names(.)) %>%
      pivot_wider(., names_from = feat, values_from = .) %>%
      mutate_at(vars(contains('id')), as.integer) %>%
      rename_with(~ if_else(stringr::str_detect(.x, '^[a-z.]+1$'), sub('1', '', .x, fixed = TRUE), .x)) %>%
      select(lapply(1:nr, function(x) seq(x, ncol(.), nr)) %>% unlist) %>% as.data.frame  
  }
  
  if (is.null(ESA_Matches$away_team.managers[[i]])){
    ESA_Matches$away_team.managers[[i]] = structure(data.frame(matrix(rep(NA, length(managers_data)), nrow = 1)),
                                                    names = managers_data)
  }
  nr = nrow(ESA_Matches$away_team.managers[[i]])
  if (nr > 1){
    ESA_Matches$away_team.managers[[i]] =
      ESA_Matches$away_team.managers[[i]] %>% unlist %>% data.frame(feat = names(.)) %>%
      pivot_wider(., names_from = feat, values_from = .) %>%
      mutate_at(vars(contains('id')), as.integer) %>%
      rename_with(~ if_else(stringr::str_detect(.x, '^[a-z.]+1$'), sub('1', '', .x, fixed = TRUE), .x)) %>%
      select(lapply(1:nr, function(x) seq(x, ncol(.), nr)) %>% unlist) %>% as.data.frame  
  }
}

ESA_Matches = ESA_Matches %>%
  unnest(c(home_team.managers, away_team.managers), .sep = '.')

write.table(ESA_Matches, '.csv'); ESA_Matches = read.table('.csv', fileEncoding = 'UTF-8'); file.remove('.csv')


# rozwinięcie ESA_lineups (events, positions, formations, stats)

events_data = c('period', 'timestamp', 'type', 'player_id', 'player_name', 'outcome')
positions_data = c('position_id', 'position', 'from', 'to', 'from_period', 'to_period',
                   'start_reason', 'end_reason', 'counterpart_id', 'counterpart_name')
formations_data = c('period', 'timestamp', 'reason', 'formation')
stats_data = c('own_goals', 'goals', 'assists', 'penalties_scored', 'penalties_missed', 'penalties_saved')

for (i in 1:nrow(ESA_lineups)){
  if (class(ESA_lineups$events[[i]]) == 'list'){
    ESA_lineups$events[[i]] = structure(data.frame(matrix(rep(NA, length(events_data)), nrow = 1)),
                                        names = events_data)
  } else {
    if (nrow(ESA_lineups$events[[i]]) == 0){
      ESA_lineups$events[[i]] = structure(data.frame(matrix(rep(NA, length(events_data)), nrow = 1)),
                                          names = events_data)
    }
  }
  nr = nrow(ESA_lineups$events[[i]])
  if (ncol(ESA_lineups$events[[i]]) == 3){
    ESA_lineups$events[[i]] = cbind(ESA_lineups$events[[i]],
                                    structure(data.frame(matrix(rep(NA, length(events_data[4:6]) * nr), nrow = nr)),
                                              names = events_data[4:6]))
  }
  ESA_lineups$events[[i]] = ESA_lineups$events[[i]] %>%
    relocate('type', .before = 'period') %>%
    mutate_at(vars(contains('player_id')), as.integer)
  if (nr > 1){
    ESA_lineups$events[[i]] =
      ESA_lineups$events[[i]] %>% unlist %>% data.frame(feat = names(.)) %>%
      pivot_wider(., names_from = feat, values_from = .) %>%
      mutate_at(vars(contains('period')), as.integer) %>%
      rename_with(~ if_else(stringr::str_detect(.x, '^[a-z_]+1$'), sub('1', '', .x, fixed = TRUE), .x)) %>%
      select(lapply(1:nr, function(x) seq(x, ncol(.), nr)) %>% unlist) %>%
      as.data.frame  
  } else {
    ESA_lineups$events[[i]] = ESA_lineups$events[[i]] %>%
      select(lapply(1:nr, function(x) seq(x, ncol(.), nr)) %>% unlist) %>%
      as.data.frame  
  }
  
  if (is.null(ESA_lineups$positions[[i]])){
    ESA_lineups$positions[[i]] = structure(data.frame(matrix(rep(NA, length(positions_data)), nrow = 1)),
                                           names = positions_data)
  } else {
    if (nrow(ESA_lineups$positions[[i]]) == 0){
      ESA_lineups$positions[[i]] =  structure(data.frame(matrix(rep(NA, length(positions_data)), nrow = 1)),
                                              names = positions_data)
    }
  }
  nr = nrow(ESA_lineups$positions[[i]])
  if (ncol(ESA_lineups$positions[[i]]) == 8){
          ESA_lineups$positions[[i]] = cbind(ESA_lineups$positions[[i]],
                                             structure(data.frame(matrix(rep(NA, length(positions_data[9:10]) * nr), nrow = nr)),
                                                                                   names = positions_data[9:10]))
  }
  ESA_lineups$positions[[i]] = ESA_lineups$positions[[i]] %>%
    relocate('from', .after = 'from_period') %>%
    relocate('to', .after = 'to_period') %>%
    rename(position_name = position,
           from_timestamp = from,
           to_timestamp = to)
  if (nr > 1){
    ESA_lineups$positions[[i]] =
      ESA_lineups$positions[[i]] %>% unlist %>% data.frame(feat = names(.)) %>%
      pivot_wider(., names_from = feat, values_from = .) %>%
      mutate_at(vars(contains('period')), as.integer) %>%
      mutate_at(vars(contains('id')), as.integer) %>%
      rename_with(~ if_else(stringr::str_detect(.x, '^[a-z_]+1$'), sub('1', '', .x, fixed = TRUE), .x)) %>%
      select(lapply(1:nr, function(x) seq(x, ncol(.), nr)) %>% unlist) %>%
      as.data.frame  
  }
  
  if (class(ESA_lineups$formations[[i]]) == 'list'){
    ESA_lineups$formations[[i]] = structure(data.frame(matrix(rep(NA, length(formations_data)), nrow = 1)),
                                            names = formations_data)
  } else {
    if (nrow(ESA_lineups$formations[[i]]) == 0){
      ESA_lineups$formations[[i]] = structure(data.frame(matrix(rep(NA, length(formations_data)), nrow = 1)),
                                              names = formations_data)
    }
  }  
  nr = nrow(ESA_lineups$formations[[i]])
  ESA_lineups$formations[[i]] = ESA_lineups$formations[[i]] %>%
    relocate('formation', .before = 'period')
  if (nr > 1){
    ESA_lineups$formations[[i]] =
      ESA_lineups$formations[[i]] %>% unlist %>% data.frame(feat = names(.)) %>%
      pivot_wider(., names_from = feat, values_from = .) %>%
      mutate_at(vars(contains('period')), as.integer) %>%
      rename_with(~ if_else(stringr::str_detect(.x, '^[a-z_]+1$'), sub('1', '', .x, fixed = TRUE), .x)) %>%
      select(lapply(1:nr, function(x) seq(x, ncol(.), nr)) %>% unlist) %>%
      as.data.frame  
  } else {
    ESA_lineups$formations[[i]] = ESA_lineups$formations[[i]] %>%
      mutate_at(vars(contains('formation')), as.character) %>%
      select(lapply(1:nr, function(x) seq(x, ncol(.), nr)) %>% unlist) %>%
      as.data.frame  
  }
  
  if (length(ESA_lineups$stats[[i]]) == 0){
    ESA_lineups$stats[[i]] = structure(as.list(rep(NA, length(stats_data))), names = stats_data)
  }
}

ESA_lineups = ESA_lineups %>%
  unnest_wider(stats, names_sep = '.') %>%
  unnest(c(events, positions, formations), .sep = '.') %>%
  relocate('stats.own_goals', .after = 'stats.assists')

# usunięcie prawdopodobnych błędnych danych w ESA_lineups

ESA_lineups = ESA_lineups %>%
  mutate_at(vars(starts_with('positions.end_reason')),
                        function(j) stringr::str_replace(j, 'On', 'Off'))
            
# zmiana nazw kolumn w ESA_Matches i ESA_events_clean

ESA_Matches = ESA_Matches %>%
  rename(season_id = season.season_id,
         season_name = season.season_name,
         competition_id = competition.competition_id,
         competition_name = competition.competition_name)

ESA_events_clean = ESA_events_clean %>%
  rename(event_id = index,
         team_id = team.id,
         player_id = player.id)

# dodanie kolumn do ESA_events

ESA_events_clean = ESA_events_clean %>%
  tibble::add_column(second_total_in_per = .$minute * 60 + .$second - 45 * 60 * (.$period == 2), .after = 'second') 

period_length = ESA_events_clean %>%
  group_by(match_id, period) %>%
  summarise(max_second_total_in_per = max(second_total_in_per),
            add_time_length = max_second_total_in_per - 45 * 60) %>%
  ungroup()

ESA_events_clean = merge(ESA_events_clean,
                         period_length %>%
                           filter(period == 1) %>%
                           select(match_id, max_second_total_in_per), by = 'match_id') %>% 
  add_column(second_total = .$second_total_in_per + .$max_second_total_in_per * (.$period == 2), .after = 'second_total_in_per') %>%
  select(-max_second_total)



#### Zapis danych po modyfikacji ###

save(ESAComps,
     ESA_Matches,
     ESA_lineups,
     ESA_events_clean,
     period_length,
file = 'data_after_N.RData')
load('data_after_N.RData')



### Wybranie istotnych zmiennych ###

ESAComps_to_agg = ESAComps %>% select(c(season_id,
                                        competition_id,
                                        competition_international))
ESA_Matches_to_agg = ESA_Matches %>% select(c(match_id,
                                              competition_id,
                                              season_id,
                                              competition_stage.id,
                                              match_week,
                                              match_date,
                                              referee.id,
                                              home_team.home_team_id,
                                              starts_with('home_team.managers.id'),  
                                              away_team.away_team_id,
                                              home_team.home_team_id,
                                              starts_with('away_team.managers.id'),  
                                              home_score,
                                              away_score))
ESA_lineups_to_agg = ESA_lineups %>% select(c(player_id,
                                              birth_date,
                                              team_id,
                                              match_id,
                                              starts_with('formations'),
                                              starts_with('positions'),
                                              !starts_with('positions.counterpart_name'),
                                              'stats.goals',
                                              'stats.assists',
                                              starts_with('events'),
                                              !starts_with('events.player_name')))
ESA_events_to_agg = ESA_events_clean %>% select(c(event_id,
                                                  period,
                                                  timestamp,
                                                  minute,
                                                  second,
                                                  possession,
                                                  type.id,
                                                  possession_team.id,
                                                  team_id,
                                                  player_id,
                                                  foul_committed.advantage,
                                                  foul_committed.card.id,
                                                  substitution.outcome.id, 
                                                  substitution.outcome.name, 
                                                  substitution.replacement.id, 
                                                  substitution.replacement.name,
                                                  match_id,
                                                  foul_committed.offensive,
                                                  injury_stoppage.in_chain,
                                                  location.x,
                                                  location.y))



### Zapis danych po wybraniu istotnych zmiennych ###

save(ESAComps_to_agg,
     ESA_Matches_to_agg,
     ESA_lineups_to_agg,
     ESA_events_to_agg,
file = 'data_to_agg_N.RData')
load('data_to_agg_N.RData')



### Stworzenie jednej wspólnej tabeli ###   

data_all = merge(x = ESAComps_to_agg,
                 y = ESA_Matches_to_agg,
                 by = c('season_id', 'competition_id')) %>%
           merge(x = .,
                 y = ESA_lineups_to_agg,
                 by = 'match_id') %>%
           merge(x = .,
                 y = ESA_events_to_agg,
                 by = c('player_id', 'match_id'))



### Zapis danych po stworzeniu jednej wspólnej tabeli ###

save(data_all,
file = 'data_all_N.RData')
load('data_all_N.RData')



### Podgląd danych ###

View(data_all)
dplyr::glimpse(data_all)      
skimr::skim(data_all)
summarytools::dfSummary(data_all, round.digits = 2) %>%
  summarytools::view(method = 'browser')

summarytools::dfSummary(data_all, round.digits = 2) %>%
  summarytools::view(method = 'browser', file = 'data_all_summary.html')

summarytools::dfSummary(ESAComps, round.digits = 2, max.distinct.values = 15) %>%
  summarytools::view(method = 'browser', file = 'ESAComps_summary.html')

summarytools::dfSummary(ESA_Matches, round.digits = 2, max.distinct.values = 15) %>%
  summarytools::view(method = 'browser', file = 'ESA_Matches_summary.html')

summarytools::dfSummary(ESA_lineups, round.digits = 2, max.distinct.values = 15) %>%
  summarytools::view(method = 'browser',file = 'ESA_lineups_summary.html')

#summarytools::dfSummary(ESA_events_clean, round.digits = 2, max.distinct.values = 15) %>%
#  summarytools::view(method = 'browser', file = 'ESA_events_summary.html')

View(ESA_events_clean)
dplyr::glimpse(ESA_events_clean)      
skimr::skim(ESA_events_clean)




### Usunięcie zbędnych zmiennych ###

rm(i, nr, managers_data, events_data, positions_data, formations_data, stats_data)


