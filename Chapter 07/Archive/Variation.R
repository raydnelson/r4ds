# Variation from R for Data Science Chapter 7
# Initial: September 28, 2018
# Revision: October 1, 2018
# Ray Nelson

# libraries
library(tidyverse)

# Overview of the variables in the diamonds tibble
diamonds %>% glimpse()
diamonds %>% summary()
diamonds %>% View()
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

# Size of diamonds
## Histogram
diamonds %>% ggplot(mapping = aes(x = carat)) +
  geom_histogram()

# Smaller diamonds and the size of the bin width
## Histogram
diamonds %>% filter(carat < 3) %>%
  ggplot(mapping = aes(x = carat)) +
    geom_histogram(binwidth = 0.01, fill = "lightgreen")

# Diamond size with freqpoly
## Histogram and freqpoly
## All diamonds
diamonds %>%
  ggplot(mapping = aes(x = carat)) +
    geom_histogram() +
    geom_freqpoly(color = "red", size = 2)

# Diamonds less than 3 carats
diamonds %>% filter(carat < 3) %>%
  ggplot(mapping = aes(x = carat)) +
  geom_freqpoly(color = "blue")

# Rug Plots
## Histogram and rug plots
diamonds %>% 
  ggplot(mapping = aes(x = carat)) +
    geom_histogram(fill = "lightblue") +
    geom_rug()

# all diamonds density and the influence of adjust
diamonds %>%
  ggplot(mapping = aes(x = carat)) +
    geom_density(adjust = 0.5, fill = "lightblue")

# all diamonds with boxplot
diamonds %>% 
  ggplot(mapping = aes(x = factor(0), y = carat)) +
    geom_boxplot() +
    coord_flip()

# violin plot
diamonds %>% 
  ggplot(mapping = aes(x = factor(0), y = carat)) +
    geom_violin(adjust = 3, fill = "lightblue") +
    geom_boxplot(width = 0.25, fill = "lightgreen") +
    coord_flip() +
    labs(
      title = "Violin Plot of Diamonds Weight",
      x = "",
      y = "Weight in Carats"
    )

# Violin plot for smaller diamonds 2 carats or less
diamonds %>% 
  filter(carat <= 2) %>% 
  ggplot(mapping = aes(x = factor(0), y = carat)) +
    geom_violin(adjust = 2, fill = "lightblue") +
    geom_boxplot(width = 0.25, fill = "lightgreen") +
    coord_flip() +
    labs(
      title = "Violin Plot of Diamonds Weight",
      subtitle = "Smaller Diamonds, Two Carats or Less",
      x = "",
      y = "Weight in Carats"
  )

# Old Faithful distribution of eruptions
## Information and faithful dataset

faithful %>% glimpse()
faithful %>% summary()
? faithful

## Histogram with rug plot

faithful %>%
  ggplot(mapping = aes(x = eruptions)) +
  geom_histogram()+
  geom_rug()

## Freqpoly with rug plot
faithful %>%
  ggplot(mapping = aes(x = eruptions)) +
  geom_freqpoly() +
  geom_rug()

## Density Trace and rug
faithful %>%
  ggplot(mapping = aes(x = eruptions)) +
  geom_density(fill = "hotpink")+
  geom_rug()

## Boxplot
faithful %>%
  ggplot(mapping = aes(x = factor(0), y = eruptions)) +
  geom_boxplot(fill = "lightgreen") +
  coord_flip()

## Violin plot

faithful %>%
  ggplot(mapping = aes(x = factor(0), y = eruptions)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = 0.05, fill = "lightgreen") +
  coord_flip()

# Missing values
## Histogram of y with all observations
diamonds %>% 
  ggplot(mapping = aes(x = y)) +
    geom_histogram() +
    geom_rug()

## Remove small and large y values
diamonds %>% 
  filter(!between(y, 3, 20))

## Keep only the values between 3 and 20

diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

## Turn strange values into missing values
diamonds2  <-  diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))


# Removing missing values
diamonds2 %>% 
  filter(!is.na(y))

diamonds2 %>% na.omit()

## Effect of removing strange values

diamonds %>% ggplot(mapping = aes(x = y)) +
  geom_density(fill = "lightblue")

diamonds2 %>% ggplot(mapping = aes(x = y)) +
  geom_density(fill = "lightblue")
