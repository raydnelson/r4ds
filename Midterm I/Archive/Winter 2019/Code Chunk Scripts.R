# Midterm I Winter 2019
# Initial: 12 Feb 2019
# Revision: 12 Feb 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(nycflights13)
  
# Code Intrepretation

## Code Chunch #1 (Don't worry about the labeling code)

flights %>%
  select(origin, month, day, dep_time:dep_delay) %>%
  filter(dep_delay >= 0) %>%
  group_by(origin, month, day) %>%
  summarise(average_delay = mean(dep_delay),
            number_flights = n()) %>%
  ggplot(aes(number_flights, average_delay, color = origin)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(show.legend = FALSE) +
  facet_grid(rows = vars(origin)) +
  labs(
    title = "Relationship between number of flights and average delay",
    subtitle = "Newark, La Guardia, and JFK Airports",
    x = "Number of flights",
    y = "Average delay in minutes"
  )

## Code Chunk #2 (Don't worry about the labeling code)

flights %>%
  filter(dest %in% c("LAX", "ATL", "ORD", "SLC")) %>%
  group_by(origin, dest)  %>%
  summarise(number_flights = n()) %>%
  ggplot(aes(
    x = reorder(origin, number_flights, FUN = sum),
    y = number_flights,
    fill = dest
  )) +
  geom_col() +
  labs(
    title = "Total numbers of flights leaving New York Airports",
    subtitle = "Destinations: Atlanta, Chicago, Los Angeles, and Salt Lake City",
    x = "Airport",
    y = "Number of Flights"
  )
