# Variation from R for Data Science Chapter 7
# Initial: September 28, 2018
# Revision: September 28, 2018
# Ray Nelson

# libraries
library(tidyverse)

# Overview of the variables in the diamonds tibble
diamonds %>% glimpse()
diamonds %>% summary()
? diamonds

# Variation in categorical variables
## Cut
diamonds %>% count(cut)
diamonds %>% ggplot(mapping = aes(x = cut)) +
  geom_bar(fill = "hotpink")

## Color
diamonds %>% count(color)
diamonds %>% ggplot(mapping = aes(x = color)) +
  geom_bar(fill = "lightblue")

## Clarity
diamonds %>% count(clarity)
diamonds %>% ggplot(mapping = aes(x = clarity)) +
  geom_bar(fill = "lightgreen")

# Function to look at categorical variables
categorical <- function(characteristic) {
  diamonds %>% count(get(characteristic)) %>% print()
  diamonds %>% ggplot(mapping = aes(x = get(characteristic))) +
    geom_bar(fill = "lightblue")
}

categorical(characteristic = "cut")
categorical(characteristic = "color")
categorical(characteristic = "clarity")
