flights %>%
  group_by(origin) %>% 
  summarize(destinations = n_distinct(dest)) %>% 
  arrange(destinations %>%  desc())


flights %>%
  filter(!is.na(dep_delay)) %>% 
  filter(dep_delay < 30) %>%
  filter(dep_delay > -30) %>% 
  ggplot(aes(x = fct_reorder(origin, dep_delay, median), y = dep_delay)) +
    geom_violin(fill = "gray60", bw = 0.6) +
    geom_boxplot(width = 0.10, fill = "white") +
    stat_summary(fun.y = "mean",
      geom = "point",
      shape = 18,
      size = 1.5) +
    coord_flip() +
  labs(
    title = "Distributions of departure delays for New York City Airports",
    x = "Airport",
    y = "Minutes"
  )

flights %>%
  filter(!is.na(dep_delay)) %>% 
  filter(dep_delay < 30) %>%
  filter(dep_delay > -30) %>% 
  group_by(origin) %>% 
  summarize(
    Mean = mean(dep_delay, na.rm = TRUE),
    Median = median(dep_delay, na.rm = TRUE),
    IQR = IQR(dep_delay, na.rm = TRUE),
    SD = sd(dep_delay, na.rm = TRUE)
  )

# Filtering and group by
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





