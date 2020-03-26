# Midterm II Data Science
# Initial: 23 Mar 2020
# Revision: 24 Mar 2020
# Ray Nelson

# Libraries
library(tidyverse)
library(nycflights13)
library(viridis)
library(lubridate)

# Code Chunk #1
faithful %>% 
  gather(key = time_span, value = minutes) %>% 
  group_by(time_span) %>% 
  summarise(
    Location = median(minutes),
    Scale = IQR(minutes)
  )

# Code Chunk #2
flights %>%
  filter(year == 2013, month == 3, day == 25) %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  ggplot(aes(x = lon, y = lat, size = delay, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  scale_color_viridis()

# Code chunk #3
make_datetime_100 <- function(year, month, day, time){
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
flights_dt <- flights %>%
  filter(!is.na(dep_time)) %>% 
  mutate(dep_time = make_datetime_100(year, month, day, dep_time)) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

# Code chunk #4
morning_afternoon <- function() {
  Time <- now() %>% lubridate::hour()
  if (Time < 12) {
    "It's morning"
  } else {
    "It's afternoon"
  }
}

morning_afternoon()

# Code chunk #5

data_science <- list(Step1 = "Wrangle", Step2 = "Explore", Step3 = "Communicate")
our_list <- list(one = FALSE, two = 25L, three = 2020, four = "Data Science", five = data_science)
our_list$five$Step2


# Coding Problems

# 1 Box plot ordered by IQR after gather
mpg %>% 
  select(hwy, cty) %>% 
  gather(key = "type_of_driving", value = "MPG") %>% 
    ggplot(aes(x = MPG, y = fct_reorder(type_of_driving, MPG, IQR) %>% fct_rev())) +
    geom_boxplot(fill = "lightblue") +
  labs(
    title = "Highway mileage is larger but has more variability.",
    y = ""
  )

# 2 interaction and mapping
## Map
faithful %>% map_dbl(mean)
## For loop
Means <- vector("double", length(faithful))
for(i in seq_along(faithful)){
  Means[[i]] <- mean(faithful[[i]])
}
# Print out the result
Means

# 3 left join
flights %>% 
  filter(!is.na(dep_time)) %>% 
  count(origin) %>% 
  left_join(airports, by = c("origin" = "faa")) %>% 
  select(name, n)

# 4 Function to calculate the day of the week
day_of_week <- function(date){
  date = ymd(date)
  wday(date, label = TRUE)
}

day_of_week(today())

# 5
mpg %>%
  mutate(hwy_class = cut(
    hwy,
    breaks = c(0, 18, 27, 35),
    labels = c("good", "better", "best")
  )) %>%
  select(hwy, hwy_class)

