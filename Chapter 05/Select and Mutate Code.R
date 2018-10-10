# Select and mutate illustrations
# Initial: September 24, 2018
# Revision: September 24, 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(nycflights13)

## Selection
# Select dep_time, dep_delay, arr_time, and arr_delay from flights

flights %>% select(dep_time, dep_delay, arr_time, arr_delay)
flights %>% select("dep_time", "dep_delay", "arr_time", "arr_delay")
flights %>% select(4, 6, 7, 9)

variables <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
flights %>% select(one_of(variables))

flights %>% select(starts_with("dep_"), starts_with("arr_"))

flights %>% select(contains("_time"), contains("arr_"))

## Modulus calculations

# Number of Minutes since midnight from dep time
departure_time <- flights %>% select(dep_time)
departure_time %>% summary()

# Calculate the Number of Hours
hours <- departure_time %/% 100

# Calculate the Number of Minutes
minutes <- departure_time %% 100

# Combine minutes and hours and deal with 12 Midnight
time_since_midnight <- (hours * 60 + minutes) %% 1440

# time conversion function
time_to_minutes <- function(time){
  (time %/% 100 * 60 + time %% 100) %% 1440
}

## Logarithmic Calculations
# Distance Traveled

# Create a data frame with distance and log base 10 of distance
plot_data <- flights %>%
  select(distance) %>%
  mutate(log_distance = log10(distance))

# Boxplot of distance
plot_data %>% ggplot(aes(x = factor(0), y = distance)) +
  geom_boxplot()

# Boxplot of log distance

plot_data %>% ggplot(aes(x = factor(0), y = distance %>% log)) +
  geom_boxplot(fill = "lightgreen", width = 0.25) +
  labs(title = "Box Plot of Distance Traveled",
       y = "Log Base 10 Distance",
       x = "") +
  coord_flip()

# Offsets by using lead and lag
(x <- 1:10)

# Lag
x %>% lag()

# Lead
x %>% lead()

# Cumulataive sum, product, min, max, mean
x %>% cumsum()
x %>% cumprod()
x %>% cummin()
x %>% cummax()
x %>% cummean()

# Logical comparisons
# New variable delayed which is true or false

flights %>%
  select(dep_delay) %>% 
  mutate(departure_delay = dep_delay > 0)

# Ranking
# Data
y <- c(1, 2, 2, NA, 3, 4)

# Minimum rank
y %>% min_rank()

# Minimum rank descending
y %>% desc() %>% min_rank()
