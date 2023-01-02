
library(tidyverse)



### Tabele częstości powodów rozpoczęcia i zakończenia gry na danej pozycji ###

ESA_lineups %>%
  tidyr::pivot_longer(cols = stringr::str_subset(names(.), 'positions.start_reason'),
                      names_to = 'positions_number', values_to = 'positions.start_reason', values_drop_na = TRUE) %>%
  group_by(positions.start_reason) %>%
  summarise(n_sr = n()) %>%
  arrange(-n_sr)

ESA_lineups %>%
  tidyr::pivot_longer(cols = stringr::str_subset(names(.), 'positions.end_reason'),
                      names_to = 'positions_number', values_to = 'positions.end_reason', values_drop_na = TRUE) %>%
  group_by(positions.end_reason) %>%
  summarise(n_er = n()) %>%
  arrange(-n_er)


### Przebieg zmian danego zawodnika w ciągu meczu ###

ESA_lineups %>%
  select(lapply(seq(stringr::str_which(names(.), 'positions')[1],
                    stringr::str_which(names(.), 'positions')[length(stringr::str_which(names(.), 'positions'))], 10),
                function(x) c(x + 2, x + 3, x + 4, x + 5, x + 1, x + 6, x + 7)) %>% unlist) %>%
  filter(!is.na(positions.position2)) %>% # & (positions.end_reason != positions.start_reason2)) #%>%
  View



