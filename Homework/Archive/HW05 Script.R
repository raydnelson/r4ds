library(nycflights13)
data(flights)


spring_fall <- filter(flights, month == 5 | month == 9, carrier == "DL")
ggplot(data = filter(spring_fall, dep_delay < 90),
       mapping = aes(x = as.factor(month),
                     y = dep_delay,
                     fill = as.factor(month))) +
  geom_violin(show.legend = FALSE) +
  geom_boxplot(width = 0.05, fill = "grey", show.legend = FALSE) +
  coord_flip() +
  facet_grid(rows = vars(origin))
 
flights %>%
  filter(month == 4, origin == "JFK") %>% 
  ggplot(aes(x = distance, y = air_time)) +
  geom_point(position = "jitter", alpha = 1/100) +
  geom_smooth()

flights %>% 
  filter(carrier %in% c("DL", "UA", "AA"), dest %in% c("ORD", "LAX", "ATL")) %>% 
  ggplot(aes(x = carrier, y = dep_delay)) +
  geom_boxplot() + 
  coord_flip() +
  facet_grid(rows = vars(dest)) 

