# Problem 6 Script: Select and mutate
# Initial: 24 Jan 2019
# Revision: 24 Jan 2019
# Ray Nelson

# Libraries
library(tidyverse)
library (nycflights13)

# Number of minutes since midnight

flights %>%
  select(dep_time, origin) %>%
  mutate(
    hours = dep_time %/% 100,
    minutes = dep_time %% 100,
    minutes_after_midnight = 60 * hours + minutes
  ) %>%
  ggplot(aes(x = origin, y = minutes_after_midnight, fill = origin)) +
  geom_violin(show.legend = FALSE) +
  geom_boxplot(width = 0.1,
               fill = "grey",
               show.legend = FALSE) +
  coord_flip()
