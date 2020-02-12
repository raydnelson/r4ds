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



