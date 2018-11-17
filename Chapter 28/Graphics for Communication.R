# Chapter 28: Graphics for Communication
# Initial: 15 November 2018
# Revision: 15 November 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(ggrepel)
library(viridis)

# 28.2 Label
mpg %>%
  ggplot(aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_smooth(se = FALSE) +
    labs(title = "Fuel efficiency generally decreases with engine size")