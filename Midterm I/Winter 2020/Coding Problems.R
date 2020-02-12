# Midterm I Winter 2019
# Initial: 12 Feb 2019
# Revision: 12 Feb 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(nycflights13)

# Distribution Problem

mpg %>%
  select(cty, hwy) %>% 
  gather(key = "type", value = "mileage") %>% 
     ggplot(aes(x = type, y = mileage)) +
      geom_violin(fill = "grey") +
      geom_boxplot(width = 0.2, fill = "grey50") +
      labs(title = "Comparison of Distributions of City and Highway Mileage",
        x = "",
        y = "Miles Per Gallon") +
      coord_flip()

mpg %>%
  select(cty, hwy) %>% 
  gather(key = "type_of_driving", value = "mileage") %>% 
  group_by(type_of_driving) %>% 
  summarise(
    Mean = mean(mileage),
    Median = median(mileage),
    Standard_Deviation = sd(mileage),
    Interquartile_Range = IQR(mileage)
  )
  
# Code Interpretation

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


flights %>%
  filter(dest %in% c("LAX", "ATL", "ORD", "SLC")) %>%
  group_by(origin, dest)  %>%
  summarise(number_flights = n()) %>%
  ggplot(aes(
    x = reorder(origin, number_flights, FUN = sum),
    y = number_flights,
    fill = dest
  )) +
  geom_col()


# Code Creation

# Create a new variable called departure hour from the scheduled departure time variable
# Calculate the number of flights that leave during each hour and each airport
# Draw a bar chart for each of the three New York airports
# Don't worry about replicating the graph labels. We will cover that in Chapter 28

flights %>% 
  mutate(
    dep_hour = sched_dep_time %/% 100
  ) %>% 
  group_by(origin, dep_hour) %>%
  summarise(
    number_flights = n()
  ) %>% 
  ggplot(aes(dep_hour, number_flights, fill = origin)) +
  geom_col(show.legend = FALSE) +
  facet_grid(rows = vars(origin)) +
  labs(
    title = "Number of flights leaving each New York airport by departure hour",
    subtitle = "Departure hour calculated from the scheduled departure time",
    x = "Departure hour",
    y = "Total number of flights\n"
  )

# Number of destinations from each airport

flights %>% 
  group_by(origin) %>% 
  summarise(
    destinations = n_distinct(dest)
  ) %>% 
  ggplot(aes(x = reorder(origin, destinations, FUN = sum), y = destinations)) +
  geom_col(aes(fill = origin), show.legend = FALSE) +
  labs(
    title = "Number of distinations for each New York airport",
    subtitle = "Sorted by number of flights",
    x = "New York Airport",
    y = "Number of Destinations"
  )