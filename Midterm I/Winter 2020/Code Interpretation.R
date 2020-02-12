# Midterm I
# Initial: 10 Feb 2020
# Revision: 10 Feb 2020
# Ray Nelson

# Libraries
library(tidyverse)
library(nycflights13)

# 
flights %>% 
  filter(!is.na(dep_delay)) %>% 
  filter(origin == "JFK") %>% 
  filter(dest %in% c("ATL", "LAX", "ORD")) %>% 
  select(dest, dep_delay)  %>% 
  group_by(dest) %>% 
  summarize(departure_delay = mean(dep_delay, na.rm = TRUE))

# Proportion
flights %>% 
  group_by(origin) %>% 
  summarize(proportion = mean(dep_delay < 0, na.rm = TRUE)) %>% 
  ggplot(aes(x =  origin, y = proportion, fill = origin)) +
  geom_col(show.legend = FALSE)

# Integer arithmetic
flights %>% 
  transmute(origin, minute = dep_time %% 100)  %>% 
  ggplot(aes(x = minute, fill = origin)) +
  geom_bar(show.legend = FALSE) +
  facet_grid(rows = "origin")

# Violin Plot
flights %>% 
  filter(dep_delay < 60, dep_delay > -30) %>% 
  ggplot(aes(x = fct_reorder(origin, dep_delay, median), y = dep_delay)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = 0.1, fill = "grey60") +
  coord_flip()

flights %>%
  group_by(origin) %>% 
  summarize(destinations = n_distinct(dest)) %>% 
  arrange(destinations %>%  desc())

