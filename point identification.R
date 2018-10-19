# Point identification using plotly
# Initial: October 16, 2018
# Revision: October 17, 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(plotly)

cars <- mtcars %>% mutate(car = row.names(mtcars))

p <- cars %>% 
  ggplot(aes(x = wt, y = mpg)) +
    geom_point(aes(text = paste("Model", car)), color = "red") +
    labs(
      title = "Relationship between weight and mileage",
      subtitle = "mtcars data set",
      x = "Weight in Thousands of Pounds",
      y = "Miles Per Gallon"
    )

(gg <- ggplotly(p))
