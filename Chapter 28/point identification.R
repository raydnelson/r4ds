# Point identification using plotly
# Initial: 16 2018
# Revision: 8 Apr2019
# Ray Nelson

# Libraries
library(tidyverse)
library(plotly)

cars <- mtcars %>%
  mutate(car = row.names(mtcars))

p <- cars %>% 
  ggplot(aes(x = wt, y = mpg)) +
    geom_point(aes(text = paste("Model", car)), color = "red") +
    geom_smooth()

(gg <- ggplotly(p))
