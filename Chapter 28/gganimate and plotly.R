# gganimate and plotly
# Initial: 12 Apr 2019
# Revision: 2 Dec 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(gganimate)
library(gifski)
library(plotly)
library(gapminder)

# data in gapminder
gapminder %>% glimpse()
gapminder %>% head()

# Static plot

p <- gapminder %>%
  ggplot(aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(vars(continent))
p

# Dynamic Plot
p +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# MPG for smoothed scatterplot for the last time
p <- mpg %>% 
  select(hwy, displ, drv, model) %>% 
  ggplot(aes(displ, hwy, color = drv)) +
  geom_point(aes(text = paste("Model", model))) +
  geom_smooth() +
  labs(
    title = "The drive train appears to strongly impact fuel efficiency.",
    subtitle = "Loess Smoothed MPG Data Tibble",
    x = "Engine Displacement",
    y = "Highway Miles Per Gallon",
    color = "Type of\n Drive Train"
  )
p

p %>% ggplotly()

# Boxplot of highway and city
p <- mpg %>% 
  select(hwy, cty) %>% 
  gather(key = "Type", value = "MPG") %>% 
  ggplot(aes(Type, MPG, fill = Type)) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip()

p %>% ggplotly()

# Air quality
p <- airquality %>% ggplot(
  aes(Day, Temp, group = Month, color = factor(Month))) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  labs(x = "Day of Month",
       y = "Temperature",
       color = "Month") +
  theme(legend.position = "top")
p

p + transition_reveal(Day)
