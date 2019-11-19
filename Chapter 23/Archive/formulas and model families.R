# Model Families
# Initial: 27 Mar 2019
# Revision: 28 Mar 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(modelr)
library(MASS)

# Function definitions ---------------------------------------------------------
## Violin plot
violin_plot <- function(data_set, x, y) {
  x <- data_set[[x]]
  y <- data_set[[y]]
  tibble(x = x, y = y) %>%
    ggplot(aes(reorder(x, y, FUN = median), y)) +
    geom_violin(fill = "lightgreen") +
    geom_boxplot(width = 0.10, fill = "lightblue") +
    labs(title = "Violin Plot",
         x = "Categorical Explanatory Variable",
         y = "Response Variable")
}

## Relationship between response and explanatory
relationship <- function(data_set, x, y) {
  data_set %>%
    ggplot(aes_string(x, y)) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_smooth(colour = "red") +
    labs(title = "Relationship between response and explanatory variables",
         x = "Explanatory Variable",
         y = "Response Variable")
}

## main effects
graph_main_effects <- function(data_set, model, category, y) {
  data_set %>% 
    add_predictions(model = model, var = "pred") %>% 
    ggplot(aes_string(x = category, y = y, color = x)) +
    geom_point(show.legend = FALSE, position = "jitter") +
    geom_point(aes(y = pred), show.legend = FALSE, size = 5,
               color = "black", shape = 3,) +
    labs(
      title = "Visualization of different models.",
      x = "Explanatory Variable",
      y = "Response Variable"
    )
}

## Analysis of covariance
graph_predictions <- function(data_set, model, x, y, category) {
  data_set %>% 
    add_predictions(model = model, var = "pred") %>% 
    ggplot(aes_string(x = x, y = y, color = category)) +
    geom_point() +
    geom_line(aes(y = pred)) +
    labs(
      title = "Visualization of different models.",
      x = "Explanatory Variable",
      y = "Response Variable"
    )
}

# MPG Data Set -----------------------------------------------------------------
mpg %>% glimpse()

## Differences in hwy conditioned on the type of drive train of the vehicle
violin_plot(mpg, "drv", "hwy")

## Simple model with class as a predictor of highway miles per gallon
zero_slope <- lm(hwy ~ drv, data = mpg)
zero_slope %>% summary()
graph_main_effects(mpg, zero_slope, "drv", "hwy")

## Does displacement have an effect on mileage?
relationship(mpg, "displ", "hwy")

## Common slope for displacement and different intercepts based on drive train of vehicle
common_slope <- lm(hwy ~ displ + drv, data = mpg)
common_slope %>% summary()
graph_predictions(mpg, common_slope, "displ", "hwy", "drv")

## Different slopes and intercepts for each drive train of vehicle
different_slopes <- lm(hwy ~ displ + drv + displ * drv, data = mpg)
different_slopes %>% summary()
graph_predictions(mpg, different_slopes, "displ", "hwy", "drv")

# Diamonds ----- ---------------------------------------------------------------
## Select a subset of the diamonds by taking a random sample of 1000
data(diamonds)
diamonds_subset <- diamonds %>% sample_n(1000)
diamonds_subset %>% glimpse()

## Differences in price conditioned on the cut of the diamonds
violin_plot(diamonds_subset, "cut", "price")

## Simple model with type of motor as a predictor of highway miles per gallon
zero_slope <- lm(price ~ cut, data = diamonds_subset)
zero_slope %>%  summary()
graph_main_effects(diamonds_subset, zero_slope, "cut", "price")

## Does diamond weight have an effect?
relationship(diamonds_subset, "carat", "price")

## Common slope for displacement and different intercepts based on class of vehicle
common_slope <- lm(price ~ carat + cut, data = diamonds_subset)
common_slope %>% summary()
graph_predictions(diamonds_subset, common_slope, "carat", "price", "cut")

## Different slopes and intercepts for each class of vehicle
different_slopes <- lm(price ~ carat + cut + carat * cut, data = diamonds_subset)
different_slopes %>% summary()
graph_predictions(diamonds_subset, different_slopes, "carat", "price", "cut")

# mtcars data frame ------------------------------------------------------------
## Create factors
data(mtcars)
mtcars$vs <- factor(mtcars$vs, levels = c(0, 1), labels = c("v-shaped", "straight"))
mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c("automatic", "manual"))
glimpse(mtcars)

## Type of motor ---------------------------------------------------------------

## Differences in mpg conditioned on the type of motor of the vehicle
violin_plot(mtcars, "vs", "mpg")

## Simple model with type of motor as a predictor
zero_slope <- lm(mpg ~ vs, data = mtcars)
zero_slope %>%  summary()
graph_main_effects(mtcars, zero_slope, "vs", "mpg")

## Does weight have an effect?
relationship(mtcars, "wt", "mpg")

## Common slope for displacement and different intercepts based on type of motor
common_slope <- lm(mpg ~ wt + vs, data = mtcars)
common_slope %>% summary()
graph_predictions(mtcars, common_slope, "wt", "mpg", "vs")

## Different slopes and intercepts for each type of motor
different_slopes <- lm(mpg ~ wt + vs + wt * vs, data = mtcars)
different_slopes %>% summary()
graph_predictions(mtcars, different_slopes, "wt", "mpg", "vs")

## Type of transmission --------------------------------------------------------

## Differences in mpg conditioned on the transmission of the vehicle
violin_plot(mtcars, "am", "mpg")

## Simple model with transmission as a predictor of highway miles per gallon
zero_slope <- lm(mpg ~ am, data = mtcars)
zero_slope %>%  summary()
graph_main_effects(mtcars, zero_slope, "am", "mpg")

## Does displacement have an effect on mileage?
relationship(mtcars, "disp", "mpg")

## Common slope for displacement and different intercepts based on transmission
common_slope <- lm(mpg ~ disp + am, data = mtcars)
common_slope %>% summary()
graph_predictions(mtcars, common_slope, "disp", "mpg", "am")

## Different slopes and intercepts for each type of transmission
different_slopes <- lm(mpg ~ disp + am + disp * am, data = mtcars)
different_slopes %>% summary()
graph_predictions(mtcars, different_slopes, "disp", "mpg", "am")

## Choose among all the variables and interactions

big_model <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am, data = mtcars)
big_model %>% summary()
big_model %>% anova()
big_model %>% stepAIC()

## Model with interactions

interactions_model <- lm(mpg ~ wt + qsec + am + wt * am + qsec * am, data = mtcars)
interactions_model %>% summary()
interactions_model %>% anova()
interactions_model %>% stepAIC()

