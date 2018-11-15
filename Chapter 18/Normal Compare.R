#  Normal Comparison
# Initial: 31 October 2018
# Revision: 31 October 2018
# Ray Nelson

# Libraries
library(tidyverse)

# Range of values
x <- seq(-10, 10, 0.1)

# Standard normal
standard <- dnorm(x, mean = 0, sd = 1)

# Normal distribution
normal <- dnorm(x, mean = 5, sd = 0.5)

data.frame(x, standard, normal) %>%
  gather(standard, normal, key = "distribution", value = "probability") %>% 
  ggplot(aes(x = x, y = probability, fill = distribution)) +
    geom_ribbon(aes(ymin = 0, ymax = probability))

data.frame(x, standard, normal) %>% 
  ggplot(aes(x = x)) +
    geom_ribbon(aes(ymin = 0, ymax = standard), fill = "lightblue", alpha = 0.5) +
    geom_ribbon(aes(ymin = 0, ymax = normal), fill = "lightgreen", alpha = 0.5)


# Compare function -------------------------------------------------------------
compare <- function(mu, sigma, minimum, maximum){
  # Create the pdfs
  x <- seq(minimum, maximum, 0.1)
  standard <- dnorm(x, mean = 0, sd = 1)
  normal <- dnorm(x, mean = mu, sd = sigma)
  
  # Plot the pdfs
  data.frame(x, standard, normal) %>% 
    ggplot(aes(x = x)) +
    geom_ribbon(aes(ymin = 0, ymax = standard), fill = "blue", alpha = 0.25) +
    geom_line(aes(y = standard), color = "blue") +
    geom_ribbon(aes(ymin = 0, ymax = normal), fill = "red", alpha = 0.25) +
    geom_line(aes(y = normal), color = "red")
}

compare(mu = 1, sigma = 2, minimum = -5, maximum = 8)
