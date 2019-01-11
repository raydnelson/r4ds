# Homework #2 Script
# Initial: 11 Jan 2019
# Revision: 11 Jan 2019
# Ray Nelson

# Libraries
library(tidyverse)

# Code to interpret

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = class)) +
  geom_point(position = "jitter", show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, color = "black", size = 0.50) +
  geom_smooth(se = FALSE, show.legend = FALSE) +
  facet_wrap(vars(class))

# Code to write

