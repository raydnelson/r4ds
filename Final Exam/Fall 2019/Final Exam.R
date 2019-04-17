# Final Exam: Moview Dataset
# Initial: 13 December 2018
# Revision: 13 December 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(readxl)

# Read in data
movies <- read_excel("Final Exam/Movies.xlsx")

# Comparison of revenue by rating
movies %>%
   ggplot(aes(x = rating, y = domestic)) +
    geom_violin(fill = "lightgreen") +
    geom_boxplot(width = 0.15, fill = "lightblue") +
    labs(title = "Different Distributions",
      x = "Movie Rating",
      y = "Domestic Gross Revenue") +
    coord_flip()

# Comparison of revenue with the Titanics Movie

movies %>% 
  filter(movie != "Titanic") %>% 
  ggplot(aes(x = rating, y = domestic)) +
    geom_violin(fill = "lightgreen") +
    geom_boxplot(width = 0.15, fill = "lightblue") +
    labs(title = "Different Distributions",
         x = "Movie Rating",
         y = "Domestic Gross Revenue") +
    coord_flip()

# Scatterplot of Domestic and Worldwide Revenue

movies %>% 
  filter(Movie != "Titanic") %>% 
  ggplot(aes(x = domestic, y = worldwide, color = Rating)) +
    geom_point() +
    geom_smooth(method = "lm")


lm(Worldwide ~ domestic + type, data = movies) %>% summary()

# Distribution of domestic revenue
movies %>%
   ggplot(aes(x = factor(0), y = domestic)) +
    geom_violin(fill = "lightgreen") +
    geom_boxplot(width = 0.25, fill = "lightblue") +
    labs(title = "There are a signficant number of outliers",
      x = "",
      y = "Millions of Dollars") +
    coord_flip()

# Distribution of domestic revernue by rating

movies %>%
   ggplot(aes(x = rating, y = domestic)) +
    geom_violin(fill = "lightgreen") +
    geom_boxplot(width = 0.1, fill = "lightblue") +
    labs(title = "Domestic Revenue",
      x = "Movie Rating",
      y = "Millions of Dollars") +
    coord_flip()

# Distribution of domestic revernue by type

movies %>%
  ggplot(aes(x = type, y = domestic)) +
    geom_violin(fill = "lightgreen") +
    geom_boxplot(width = 0.1, fill = "lightblue") +
    labs(title = "Domestic Revenue by Type",
         x = "Movie Rating",
         y = "Millions of Dollars") +
    coord_flip()

# Summary statistics for type

movies %>% 
  group_by(type) %>% 
  summarise(
    type_mean = mean(domestic),
    type_median = median(domestic),
    type_sd = sd(domestic),
    type_iqr = IQR(domestic)
  )

# Summary statistics for rating

movies %>% 
  group_by(rating) %>% 
  summarise(
    rating_mean = mean(domestic),
    rating_median = median(domestic),
    rating_sd = sd(domestic),
    rating_iqr = IQR(domestic)
  )

# Distribution of domestic revernue by type

movies %>%
  ggplot(aes(x = type, y = domestic)) +
    geom_violin(fill = "lightgreen") +
    geom_boxplot(width = 0.1, fill = "lightblue") +
    labs(title = "Domestic Revenue by Type",
         x = "Movie Rating",
         y = "Millions of Dollars") +
    coord_flip()

# Remove top 20 gross movies

movies %>% 
  mutate(ranking = min_rank(desc(domestic))) %>% 
  arrange(ranking) %>% 
  head(20)

# Function to scale a variable by subtracting the median and dividing by the IQR

scaled <- function(x){
 (x - median(x)) / IQR(x)
}

movies %>% 
  ggplot(aes(x = domestic, y = worldwide)) +
    geom_point()

movies %>% 
  ggplot(aes(x = scaled(domestic), y = scaled(worldwide))) +
    geom_point()

movies %>%
  mutate(scaled_domestic = scaled(domestic), scaled_worldwide = scaled(worldwide)) %>% 
  filter(scaled_domestic <= 3) %>% 
  ggplot(aes(x = scaled_domestic, y = scaled_worldwide, color = type)) +
    geom_point() +
    geom_smooth(method = "lm")


# Looping and interation

revenue <- movies %>% 
  select(domestic, worldwide)

output <- vector("double", 2)

for(i in seq_along(revenue)){
  output[[i]] <- mean(scaled(revenue[[i]]))
}

output %>% print()


# Exclude the top 20 gross movies and run a regression

regular_movies <- movies %>% 
  mutate(ranking = min_rank(desc(domestic))) %>% 
  filter(ranking > 20)

regular_movies %>% 
  ggplot(aes(x = domestic, y = worldwide, color = rating)) +
    geom_point() +
    geom_smooth(method = "lm")

lm(worldwide ~ domestic, data = regular_movies) %>%
  summary()
  
lm(worldwide ~ domestic + rating, data = regular_movies) %>%
  summary()

lm(worldwide ~ domestic * rating, data = regular_movies) %>%
  summary()

