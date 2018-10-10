# Filtering and Arranging
# Initial: September 19, 2018
# Revision: September 19, 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(nycflights13)

# Illustration #1 New Years Flight
new_years <- flights %>% filter(month == 1, day == 1)
ggplot(data = new_years, aes(x = origin, y = dep_delay)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(fill = "lightgreen", width = 0.10) +
  labs(
    title = "New York Departure Delays",
    subtitle = "For Newark, JFK, and LaGuardia",
    x = "Airport",
    y = "Minutes Delayed"
  ) +
  coord_flip()

new_years <- new_years %>% filter(dep_delay <= 90)
ggplot(data = new_years, aes(x = origin, y = dep_delay)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(fill = "lightgreen", width = 0.10) +
  labs(
    title = "New York Departure Delays",
    subtitle = "For Newark, JFK, and LaGuardia",
    x = "Airport",
    y = "Minutes Delayed"
  ) +
  coord_flip()

# Illustration # 2 New Years or Christmas flights

holiday <- flights %>% filter((month == 1 & day == 1) | (month == 12 & day == 25))
holiday <- holiday %>% filter(dep_delay < 60)

ggplot(data = holiday, aes(x = origin, y = dep_delay)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(fill = "lightgreen", width = 0.10) +
  labs(
    title = "Holiday New York Departure Delays",
    subtitle = "For Newark, JFK, and LaGuardia",
    x = "Airport",
    y = "Minutes Delayed"
  ) +
  coord_flip()

# Illustration #3 Spring Departures
spring <- flights %>% filter(month %in% 3:5)
spring %>% ggplot(mapping = aes(x = origin, fill = as.factor(month))) +
  geom_bar(position = "dodge", show.legend = FALSE) +
  labs(
    title = "Spring Departures by New York Airport",
    subtitle = "March, April, May",
    x = "",
    y = "Number of Flights"
  )

# Illustration #4
three_airports <- flights %>% filter(dest %in% c("ATL", "LAX", "ORD") & arr_delay < 360)
three_airports %>% ggplot(aes(x = arr_delay)) +
  geom_density(aes(fill = dest)) +
  geom_vline(xintercept = 0) +
  facet_grid(rows = vars(dest)) +
  labs(
    title = "Arrival Delays at Atlanta, Los Angeles, and Chicago",
    subtitle = "Flights Arrived Less than 3 Hours Late",
    x = "Arrival Delay in Minutes",
    y = "",
    fill = "Airport"
  )

# Illustration #5
not_late <- flights %>% filter(dep_delay <= 0 & carrier %in% c("AA", "UA", "DL"))
ggplot(data = not_late, mapping = aes(x = carrier, y = arr_delay, fill = carrier)) +
  geom_violin(show.legend = FALSE) +
  geom_boxplot(width = .4, fill = "grey") +
  geom_hline(yintercept = 0, color = "red") +
  labs(
    title = "Distribution of Arrival Delays",
    subtitle = "American, Delta, and United",
    x = "Carrier",
    y = "Arrival Delay in Minutes",
    fill = "Carrier"
  ) +
  coord_flip()


           