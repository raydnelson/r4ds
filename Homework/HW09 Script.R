# Homework #9 Scipt
# Initial: 6 Feb 2019
# Revision: 6 Feb 2019
# Ray Nelson

# Libraries
library(tidyverse)

cov(mpg$hwy, mpg$cty)
cor(mpg$hwy, mpg$cty)

mpg %>% 
  ggplot(aes(x = cty, y = hwy)) +
  geom_point() +
  geom_smooth(color = "blue") +
  geom_smooth(method = "lm", color = "red")

mpg %>%
  ggplot(aes(x = cty, y = hwy)) +
  stat_density_2d(aes(fill = stat(density)), geom = "raster", contour = FALSE) +
  labs(
    title = "Relationship between Old Faithful waiting time and eruption time",
    x = "Waiting Time in Minutes",
    y = "Eruption Duration in Minutes"
  )