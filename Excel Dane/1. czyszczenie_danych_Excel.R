
library(tidyverse)

setwd('C:/Users/Agnieszka/Desktop/Studia/Przedmioty/Seminarium magisterskie/Kody/Excel Dane')


### Wczytanie danych ###

path_Excel = '../Dane_xlsx'

for (i in str_subset(list.files(path_Excel), 'Players')){
  assign(str_c('ESA_Players_', str_sub(i, str_locate(i, 'Players_ ')[,2] + 1, str_locate(i, ' 2021')[,1] - 1) %>%
           str_replace_all(' - ', '_') %>%
           str_replace_all(' ', '.')),
         readxl::read_xlsx(i, sheet = 1))
}

for (i in str_subset(list.files(path_Excel), 'Linups')){
  assign(str_c('ESA_Lineups_', str_sub(i, str_locate(i, 'Linups_ ')[,2] + 1, str_locate(i, ' 2021')[,1] - 1) %>%
                 str_replace_all(' - ', '_') %>%
                 str_replace_all(' ', '.')),
         readxl::read_xlsx(i, sheet = 1))
}

gdata::mv(from = 'ESA_Players_Scotland_Premiership.', to = 'ESA_Players_Scotland_Premiership')
gdata::mv(from = 'ESA_Lineups_Scotland_Premiership.', to = 'ESA_Lineups_Scotland_Premiership')




### Zapis danych po pobraniu ###

save(list = ls(pattern = 'ESA_Players_'),
file = 'ESA_Players.RData')
load('ESA_Players.RData')

save(list = ls(pattern = 'ESA_Lineups_'),
file = 'ESA_Lineups.RData')
load('ESA_Lineups.RData')




### Podgląd danych ###

# ESA_lineups

league = ESA_Lineups_Italy_Serie.B

knitr::kable(league) %>% kableExtra::kable_styling(font_size = 12)
dplyr::glimpse(league)      
skimr::skim(league)
summarytools::dfSummary(league, round.digits = 2) %>%
  summarytools::view(., footnote = '', custom.css = 'tiny_text.css', table.classes = 'tiny-text')




### Modyfikacja danych ###

# zmiana kolejności kolumn i typów niektórych zmiennych

all_lineups_names = ls(pattern = 'ESA_Lineups_')
all_lineups_objects = mget(all_lineups_names)

plyr::l_ply(1:length(all_lineups_objects), function(i) {
  assign(x = names(all_lineups_objects)[i],
         value = all_lineups_objects[[i]] %>%
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
                             'positions',
                             'cards')) %>%
                    mutate(player_id = as.integer(player_id),
                           jersey_number = as.integer(jersey_number)),
         envir = .GlobalEnv)
})

# rozwinięcie ESA_lineups (cards i positions)

cards_data = c('timestamp', 'card_type', 'reason', 'period')
positions_data = c('position_id', 'position_name', 'from_timestamp', 'to_timestamp', 'from_period', 'to_period', 'start_reason', 'end_reason')

all_lineups_objects = mget(all_lineups_names)

plyr::l_ply(1:length(all_lineups_objects), function(i) {
  assign(x = names(all_lineups_objects)[i],
         value = all_lineups_objects[[i]] %>%
  mutate(cards =                                                                                                             # zamiana kolejności w stringach, tak, by dane dotyczące 
           sapply(1:nrow(.), function(j)                                                                                     #  dotyczące jednej kartki/pozycji była po sobie
             as.vector(sapply(1:(length(unlist(str_split(.$cards[j], ', ')))/length(cards_data)),
                              function(x) unlist(str_split(.$cards[j], ', '))[seq(x,
                                                                                  length(unlist(str_split(.$cards[j], ', '))),
                                                                                  length(unlist(str_split(.$cards[j], ', ')))/length(cards_data))])) %>%
               str_c(collapse = ', ')),
         positions = 
           sapply(1:nrow(.), function(j)
             as.vector(sapply(1:(length(unlist(str_split(.$positions[j], ', ')))/length(positions_data)),
                              function(x) unlist(str_split(.$positions[j], ', '))[seq(x,
                                                                                      length(unlist(str_split(.$positions[j], ', '))),
                                                                                      length(unlist(str_split(.$positions[j], ', ')))/length(positions_data))])) %>%
               str_c(collapse = ', '))) %>%
  
  tidyr::separate(col = 'cards',                                                                                             # rozwinięcie zagnieżdżonych zmiennych (cards i positions)                      
                  into = str_c('cards.',
                               str_c(rep(cards_data,
                                         ((max(sapply(.$cards, function(n) str_count(n, ',')), na.rm = TRUE) + 1) / length(cards_data))),
                                     rep(c('', 2:((max(sapply(.$cards, function(n) str_count(n, ',')), na.rm = TRUE) + 1) / length(cards_data))),
                                         each = length(cards_data)))),                
                  sep = ', ') %>%
  tidyr::separate(col = 'positions',                                   
                  into = str_c('positions.',
                               str_c(rep(positions_data,
                                         ((max(sapply(.$positions, function(n) str_count(n, ',')), na.rm = TRUE) + 1) / length(positions_data))),
                                     rep(c('', 2:((max(sapply(.$positions, function(n) str_count(n, ',')), na.rm = TRUE) + 1) / length(positions_data))),
                                         each = length(positions_data)))),                
                  sep = ', ') %>%
  
  mutate_at(vars(starts_with('cards.period')), as.integer) %>%                                                               # zmiana typów niektórych zagnieżdżonych wcześniej zmiennych
  mutate_at(vars(starts_with('positions.') & contains('period')), as.integer) %>%
  mutate_at(vars(starts_with('position_id')), as.integer) %>%
  
  select(1:(str_which(names(.), 'positions')[1] - 1),                                                                        # zmiana kolejności zagnieżdżonych wcześniej zmiennych
         as.vector(sapply(0:(length(str_which(names(.), 'positions'))/length(positions_data) - 1),                           #  dla positions: from_period przed from_timestamp i to_period przed to_timestamp
                          function(j) (str_which(names(.), 'positions')[1] - 1) + c(1:2, 5, 3, 6, 4, 7:8) + j * length(positions_data))),
         as.vector(sapply(0:(length(str_which(names(.), 'cards'))/length(cards_data) - 1),                                   #  dla cards: card_type, period, timestamp, reason
                          function(j) (str_which(names(.), 'cards')[1] - 1) + c(2, 4, 1, 3) + j * length(cards_data)))),
 envir = .GlobalEnv)
})

"
# usunięcie prawdopodobnych błędnych danych

all_lineups_objects = mget(all_lineups_names)

plyr::l_ply(1:length(all_lineups_objects), function(i) {
  assign(x = names(all_lineups_objects)[i],
         value = all_lineups_objects[[i]] %>%
  mutate_at(vars(starts_with('positions.end_reason')),
            function(j) stringr::str_replace(j, 'On', 'Off')),
  envir = .GlobalEnv)
})
"




### Podgląd danych ###

# ESA_lineups

league = ESA_Lineups_Italy_Serie.B

knitr::kable(league) %>% kableExtra::kable_styling(font_size = 12)
dplyr::glimpse(league)      
skimr::skim(league)
summarytools::dfSummary(league, round.digits = 2) %>%
  summarytools::view(., footnote = '', custom.css = 'tiny_text.css', table.classes = 'tiny-text')




#### Zapis danych po modyfikacji ###

save(list = ls(pattern = 'ESA_Lineups_'),
file = 'ESA_Lineups_after.RData')
load('ESA_Lineups_after.RData')




### Stworzenie jednej wspólnej tabeli z wszystkimi ligami ###

all_lineups_objects = mget(all_lineups_names)

all_lineups = data.table::rbindlist(all_lineups_objects, fill = TRUE, idcol = 'league')




####  Zapis danych po stworzeniu wspólnego ESA_lineups ###

save(all_lineups,
file = 'ESA_Lineups_all.RData')
load('ESA_Lineups_all.RData')




### Usunięcie niepotrzebnych zmiennych ###

rm(cards_data, positions_data)

