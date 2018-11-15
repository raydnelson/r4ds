# Functions
# Initial: October 30, 2018
# Revision: October 31, 2018
# Ray Nelson

# Libraries --------------------------------------------------------------------
library(tidyverse)
library(moments)

# Scaling function -------------------------------------------------------------
## Create x vector
set.seed(1234)
x <- rnorm(10)

## Calculations
x_min <- x %>% min()
x_max <- x %>% max()

x
(x - x_min) / (x_max - x_min)

## Create the function using RStudio

rescale01 <- function(x) {
  x_min <- x %>% min()
  x_max <- x %>% max()
  
  (x - x_min) / (x_max - x_min)
}

## Use the function
(x <- rnorm(10, mean = 10, sd = 10))
x %>% rescale01()

x %>% rescale01() %>% 
  mean()

x %>% rescale01() %>% 
  sd()

# Standardize a variable

# Create data
(x <- rnorm(100000, mean = 10, sd = 10))

(x %>% mean())
(x %>% sd())
(x_standard <- (x - mean(x)) / sd(x) )
x_standard %>% mean()
x_standard %>% sd()

(x_scale <- scale(x))
x_scale %>%
  mean() %>%
  round()
x_scale %>% sd()

# Judge the symmetry of a variable

range_x <- seq(0, 10, 0.1)
lognormal_pdf <- dlnorm(range_x)

data.frame(range_x, lognormal_pdf) %>% 
  ggplot(aes(x = range_x, y = lognormal_pdf)) +
    geom_ribbon(aes(ymin = 0, ymax = lognormal_pdf), fill = "lightgreen") +
    geom_line()

x <- rlnorm(10)
x %>% median()
x %>% mean()

mean(x) - median(x)

symmetry <- function(x){
  mean(x) - median(x)
}

symmetry(x)

#
set.seed(1234)
x_normal <- rnorm(100)
x_normal %>% symmetry()
x_normal %>% skewness()

# Function to check on symmetry
symmetry2 <- function(x) {
  require(moments)
  x %>% symmetry() %>% print()
  x %>% skewness() %>% print()
  data.frame(x) %>%
    ggplot(aes(x = factor(0), y = x)) +
      geom_violin(fill = "lightgreen") +
      geom_boxplot(fill = "lightblue", width = .25) +
    coord_flip() +
    labs(
      x = "",
      y = "X"
    )
}

rnorm(100) %>% symmetry2()
rlnorm(100) %>% symmetry2()

# Comparison of normal and t distributions -------------------------------------

# Range of x values
x <- seq(-5, 5, 0.1)

# Normal Distribution
normal <- dnorm(x)

# Student's t distribution
df <- 10
t <- dt(x, df = df)

plot_data <- data.frame(x, t, normal) %>% 
  gather(normal, t, key = distribution, value = probability)

plot_data %>% 
  ggplot(aes(x = x, y = probability, fill = distribution)) +
    geom_ribbon(aes(ymin = 0, ymax = probability), alpha = 0.50) +
    geom_line() +
    labs(
      title = "Effect of Degress of Freedom on Student-t Distribution",
      subtitle = "Standard Normal versus the Student-t Distribution",
      x = paste0("Degress of Freedom: ", df),
      y = "Probability",
      fill = "Density Functions"
    ) +
    scale_fill_brewer(palette = "Set1")

# Function for comparison -----------------------------------------------------
dist_compare <- function(df){
  # Range of x values
  x <- seq(-5, 5, 0.1)
  
  # Normal Distribution
  normal <- dnorm(x)
  
  # Student's t distribution
  t <- dt(x, df = df)
  
  # Create the data frame used for plotting
  
  plot_data <- data.frame(x, normal, t) %>% 
    gather(normal, t, key = distribution, value = probability)
 
  # plot of  
  plot_data %>% 
    ggplot(aes(x = x, y = probability, fill = distribution)) +
    geom_ribbon(aes(ymin = 0, ymax = probability), alpha = 0.5) +
    geom_line() +
    labs(
      title = "Effect of Degress of Freedom on Student-t Distribution",
      subtitle = "Standard Normal versus the Student-t Distribution",
      x = paste0("Degress of Freedom: ", df),
      y = "Probability",
      fill = "Density Functions"
    ) +
    scale_fill_brewer(palette = "Set1")
}

# Run the function
dist_compare(5)
