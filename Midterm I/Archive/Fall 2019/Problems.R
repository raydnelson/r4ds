library(tidyverse)
library(nycflights13)

# Bar Chart
flights %>% 
  select(origin, carrier, dest, arr_delay) %>% 
  filter(!is.na(arr_delay)) %>% 
  filter(carrier %in% c("AA", "DL", "UA")) %>% 
  filter(dest %in% c("ATL", "LAX", "ORD")) %>% 
  group_by(origin, carrier, dest) %>%
  summarise(on_time = mean(arr_delay <= 0)) %>% 
  ggplot(aes(x = carrier, y = on_time, fill = dest)) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(origin))


# Scatterplot Smoothing
mpg %>% 
  filter(class %in% c("pickup", "suv"),
         drv != "r") %>%
  ggplot(aes(x = displ, y = hwy, fill = class)) +
  geom_smooth(linetype = "dashed",
              method = "loess",
              color = "black",
              size = 1.5,
              show.legend = FALSE,
              se = FALSE) +
  geom_smooth(aes(color = class),
              method = "lm",
              size = 1.5,
              show.legend = FALSE,
              se = FALSE) +
  geom_point(position = "jitter",
             shape = 23,
             color = "black",
             size = 2,
             stroke = 1.5,
             show.legend = FALSE) +
  facet_grid(cols = vars(class))

# Violin plot
flights %>% 
  filter(dest %in% c("ATL", "BOS")) %>%
  filter(arr_delay <= 30) %>% 
  filter(arr_delay >= -30) %>% 
  ggplot(aes(x = origin, y = arr_delay, fill = origin)) +
  geom_violin(show.legend = FALSE) +
  geom_boxplot(fill = "grey", width = 0.2, show.legend = FALSE) +
  facet_grid(rows = vars(dest)) +
  coord_flip() +
  labs(
    title = "First Coding Problem:",
    subtitle = "NYC 2013 flights within 30 minutes of scheduled arrival",
    x = "Origin",
    y = "Arrival Delay"
  )

# Ontime flights that left early by proportion
flights %>% 
  filter(!is.na(dep_delay)) %>% 
  filter(!is.na(arr_delay)) %>% 
  filter(dep_delay < 0) %>% 
  group_by(origin) %>% 
  summarise(percentage_arrival = mean(arr_delay <= 0) * 100) %>% 
  ggplot(aes(x = reorder(origin, desc(percentage_arrival)),
             y = percentage_arrival,
             fill = origin)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Second Coding Problem:",
    subtitle = "Bar chart (Pareto) of percentage of flights that left early that arrived on time.",
    x = "",
    y = "Percentage"
  )

# Comparison of shapes of distribution
mpg %>% 
  filter(class == "suv" | class == "subcompact") %>% 
  ggplot(aes(x = class, y = hwy)) +
  geom_violin(show.legend = FALSE) +
  geom_boxplot(width = 0.10, show.legend = FALSE, fill = "grey") +
  coord_flip()

# boxplot and Tukey's five number summary
mpg %>% 
  ggplot(aes(x = factor(0), y = hwy)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Boxplot and Tukey's Five Number Summary",
    x = "", 
    y = "Highway Miles Per Gallon"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
