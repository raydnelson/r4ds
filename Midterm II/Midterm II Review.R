# Midterm Review
# Initial: 6 Nov 2018
# Revision: 7 Nov 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(lubridate)
library(nycflights13)

# Two different age calculation functions
## Duration calculations
dyears(1) # duration
years(1) # period

# Duration
age <- function(birthday) {
  (today() - ymd(birthday)) %/% dyears(1)
}

age(19510414)
age("19510414")

# period
age <- function(birthday) {
  (today() - ymd(birthday)) %>% as.period() %/% years(1)
}

age(19510414)

## Interval Calculation
age <- function(birthday){
  (ymd(birthday) %--% today()) %/% years(1)
}

age(19510414)

# Create Factors

(grades <- runif(100, 0, 4) %>% round())
labels <- c("F", "D", "C", "B", "A")
(grades_factor <- factor(grades,  levels = 0:4, labels = labels))

# Switch 
setseed(1234)
x <-  rnorm(100, mean = 50000, sd = 10000)
tibble(x = x) %>%
  ggplot(aes(x = x)) +
    geom_density(fill = "lightblue")

center <- function(x, type) {
  if (type == "mean"){
    location <-  mean(x)
  } else if (type == "median"){
    location  <-  median(x)
  } else if (type == "trimmed"){
    location  <-  mean(x, trim = 0.1)
  } else {
    location  <-  "Choose mean, median, or trimmed for type"
  }
return(location)
}

center(x, "mean")

center <- function(x, type) {
  switch(type,
         mean = mean(x),
         median = median(x), 
         trimmed = mean(x, trim = 0.1)
  )
}

center(x, "mean")

# Assigning grades using the switch function
switch("4", "4" = "A", "3" = "B", "2" = "C", "1" = "D")

letter_grade <- function(gpa){
  gpa <- gpa %>% round() %>% as.character()
  switch(gpa,
         "4" = "A",
         "3" = "B",
         "2" = "C",
         "1" = "D" 
          )
}

letter_grade(3.4)

# function to say whether a time is morning or afternoon
morning_or_afternoon <- function(DateTime) {
  hour <- hour(DateTime)
  if (hour < 12){
    return("morning")
  } else {
    return("afternoon")
  }
}

morning_or_afternoon(ymd_hm("2018-11-05 9:30"))

# flights
data(flights)

# Total number of flights from each of the three New York airports
flights %>% 
  ggplot(aes(x = origin, fill = origin)) +
    geom_bar(show.legend = FALSE) +
    labs(
      title = "Total flights from each New York airport"
    )

# Total number of flights to each destination
flights %>% 
  ggplot(aes(x = dest, fill = origin)) +
    geom_bar(show.legend = FALSE) +
    coord_flip() +
    labs(
      title = "Total flights to each destination"
    )

# Total number of flights to the to top 20 destinations
flights %>% 
  group_by(dest) %>% 
  count(sort = TRUE) %>% 
  head(20) %>% 
  ggplot(aes(x = fct_reorder(dest, n), y = n, fill = dest)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(
      title = "Total number of flights to the top 20 destinations",
      x = "Destination",
      y = "Number of Flights"
    )
 
# Number of destinations by airport 
flights %>% 
  group_by(origin, dest) %>% 
  count(sort = TRUE) %>% 
  group_by(origin) %>% 
  count() %>% 
  ggplot(aes(x = origin, y = nn, fill = origin)) +
    geom_col(show.legend = FALSE) +
    scale_fill_brewer(palette = "Set3") +
    labs(
      title = "Number of Destinations by airport",
      subtitle = "Graph using geom_col"
    )

flights %>% 
  group_by(origin, dest) %>% 
  count() %>% 
  ggplot(aes(x = origin, fill = origin)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_brewer(palette = "Set3") +
    labs(
      title = "Number of Destinations by airport",
      subtitle = "Graph using geom_bar"
    )

# Average delay at each origin airport
flights %>% 
  group_by(origin) %>% 
  summarize(average_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = origin, y = average_delay, fill = origin)) +
    geom_col(show.legend = FALSE) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Average delay for each of the New York airports"
    )

# Of the flights that leave late, what is the average delay at each origin airport
flights %>% 
  filter(dep_delay >= 0) %>% 
  group_by(origin) %>% 
  summarize(average_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = fct_reorder(origin, average_delay, .desc = TRUE),
             y = average_delay, fill = origin)) +
    geom_col(show.legend = FALSE) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Average delay for each of the New York airports",
      subtitle = "Flights that leave on time"
    )

# Top 20 origin/destination combinations
top_origin_destination <- flights %>% 
  group_by(origin, dest) %>% 
  count(sort = TRUE) %>% 
  head(20)

# Average delay by airport to the top 20 origin/destination combinations
flights %>% 
  semi_join(top_origin_destination) %>% 
  group_by(origin) %>% 
  summarise(average_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = origin, y = average_delay, fill = origin)) +
    geom_col(show.legend = FALSE) +
    labs(
      title = "Average delay by airport",
      subtitle = "Top 20 origin/destination combinations",
      x = "New York Airport",
      y = "Minutes"
    )

# Average Delay for most important routes by origin and destination
flights %>% 
  semi_join(top_origin_destination) %>% 
  group_by(origin, dest) %>% 
  summarise(average_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = origin, y = average_delay, fill = dest)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = "Set3") +
    labs(
      title = "Average delay for most important routes by origin and destination"
    )
