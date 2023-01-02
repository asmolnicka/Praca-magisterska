
library(tidyverse)


### Liczba powtórzeń tych samych zmian ###

ESA_events_clean %>%                                                                                  # najczęściej dokonywane zmiany
  filter(type.name == 'Substitution') %>%
  select(match_id, player_id, substitution.replacement.id) %>%
  group_by(player_id, substitution.replacement.id) %>%
  summarise(n_the_same_sub = n()) %>%
  arrange(-n_the_same_sub) 

ggplot(ESA_events_clean %>%                                                                           # histogram - liczba powtórzeń tych samych zmian 
         filter(type.name == 'Substitution') %>%
         select(match_id, player_id, substitution.replacement.id) %>%
         group_by(player_id, substitution.replacement.id) %>%
         summarise(n_the_same_sub = n()) ) +
  geom_bar(aes(x = n_the_same_sub), stat = 'count', colour = 'white', fill = 'dodgerblue') +
  coord_cartesian(xlim = c(1 - 0.25, 9 + 0.25)) +
  scale_x_continuous(breaks = 1:9)


### ###



