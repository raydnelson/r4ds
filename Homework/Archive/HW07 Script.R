library(tidyverse)
library(nycflights13)

# Interpret the code
flights %>%
  group_by(day, month) %>%
  summarize(cancelled = mean(is.na(dep_time)) * 100,
            delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = delay, y = cancelled)) +
  geom_point() +
  geom_smooth(color = "red") +
  labs(
    title = "Days with large average delays seem to be associated with percentage of cancelled flights",
    x = "Average Delay in Minutes",
    y = "Percentage of cancelled flights"
  )

# Non-cancelled flights
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

# Rank carriers from best to worst by their average arrival delays
not_cancelled %>% 
  group_by(carrier) %>% 
  summarize(arrival_delay = mean(arr_delay)) %>% 
  arrange(desc(arrival_delay))

# Rank destination airports from worst to best by their average arrival delays
not_cancelled %>% 
  group_by(dest) %>% 
  summarize(destination_delay = mean(arr_delay)) %>% 
  arrange(desc(destination_delay))

#  Rank NYC airports from best to worst by their average departure delays
not_cancelled %>% 
  group_by(origin) %>% 
  summarize(departure_delay = mean(dep_delay)) %>% 
  arrange(desc(departure_delay))

#  Rank carriers from best to worst by their on time percentage
not_cancelled %>% 
  group_by(carrier) %>% 
  summarize(on_time = mean(arr_delay <= 0)) %>% 
  arrange(desc(on_time))