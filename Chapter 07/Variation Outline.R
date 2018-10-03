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

# Count and Graph Variation in categorical variables
## Cut
diamonds %>% count(cut)
diamonds %>% ggplot(mapping = aes(x = cut)) +
  geom_bar(fill = "hotpink")

## Color


## Clarity

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


# Smaller diamonds and the size of the bin width
## Histogram


# Diamond size with freqpoly
## Histogram and freqpoly
## All diamonds


# Diamonds less than 3 carats


# Rug Plots
## Histogram and rug plots


# all diamonds density and the influence of adjust


# all diamonds with boxplot


# violin plot


# Violin plot for smaller diamonds 2 carats or less


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


## Density Trace and rug


## Boxplot


## Violin plot



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
