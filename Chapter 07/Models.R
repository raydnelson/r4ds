# Models
# Initial: October 4, 2018
# Revised: October 4, 2018
# Ray Nelson

# Load Libraries
library(car)
library(modelr)
library(tidyverse)

# Purpose is to price diamonds
## look at the prices of diamonds by size, clarity, color, and cut

## Sample of 2,000 diamonds from the diamonds tibble
set.seed(1234)
diamonds2 <- diamonds %>% sample_n(1000)

## Scatterplot of diamond prices and size
diamonds2 %>% ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = "Check for Linearity and Constant Variance",
    subtitle = "Simple Regression versus Locally Weighted Scatterplot Smoother",
    x = "Weight in Carats",
    y = "Price in Dollars"
  )

# Violin plot by clarity
diamonds2 %>% count(clarity)

diamonds2 %>% ggplot(aes(x = reorder(clarity, price, FUN = median), y = price)) +
  geom_violin(aes(fill = clarity), show.legend = FALSE) +
  geom_boxplot(width = 0.05, fill = "lightblue") +
  labs(title = "Possible effect of clarity on price",
  x = "Clarity",
  y = "Price") +
  coord_flip()

# Violin plot by color
diamonds2 %>% count(color)

diamonds2 %>% ggplot(aes(x = reorder(color, price, FUN = median), y = price)) +
  geom_violin(aes(fill = color), show.legend = FALSE) +
  geom_boxplot(width = 0.05, fill = "lightblue") +
  labs(title = "Possible effect of color on price",
       x = "Color",
       y = "Price") +
  coord_flip()

# Violin plot by cut
diamonds2 %>% count(cut)

diamonds2 %>% ggplot(aes(x = reorder(cut, price, FUN = median), y = price)) +
  geom_violin(aes(fill = cut), show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "lightblue") +
  labs(title = "Possible effect of cut on price",
       x = "Cut",
       y = "Price") +
  coord_flip()

# We have discovered that cut and clarity don't match our expectations
# Let's look at size by cut
diamonds2 %>% count(cut)

diamonds2 %>% ggplot(aes(x = reorder(cut, carat, FUN = median), y = carat)) +
  geom_violin(aes(fill = cut), show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "lightblue") +
  labs(title = "Possible effect of cut on weight",
       x = "Cut",
       y = "Weight in Carats") +
  coord_flip()

# Let's also look at size by clarity

diamonds2 %>% count(clarity)

diamonds2 %>% ggplot(aes(x = reorder(clarity, carat, FUN = median), y = carat)) +
  geom_violin(aes(fill = clarity), show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "lightblue") +
  labs(title = "Possible effect of cut on weight",
       x = "?Clarity",
       y = "Weight in Carats") +
  coord_flip()

# See if a logarithmic transformation will cause variance to be similar

diamonds2 %>% ggplot(aes(x = log(carat), y = log(price))) +
  geom_point() +
  geom_smooth()

# Fit a linear model to remove the effect on size on diamond prices
model <- lm(log(price) ~ log(carat), data = diamonds2)

# Calculate the residuals and add them to a new data frame
diamonds2 <- diamonds2 %>%
  add_residuals(model) %>% 
  mutate(resid = exp(resid))

# Scatterplot of residuals by diamond size
diamonds2 %>% ggplot(mapping = aes(x = carat, y = resid)) +
  geom_point() +
  geom_smooth()

# Violin plot of prices by cut after the effect of weight has been removed
diamonds2 %>% count(cut)
diamonds2 %>% ggplot(aes(x = reorder(cut, resid, FUN = median), y = resid)) +
  geom_violin(aes(fill = cut), show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "lightblue") +
  labs(title = "Prices with the effect of diamonds weight removed",
       x = "Cut of Diamond",
       y = "Residuals in Dollars") +
  coord_flip()

# Violin plot of prices by clarity after the effect of diamond weight has been removed
diamonds2 %>% count(clarity)

diamonds2 %>% ggplot(aes(x = reorder(clarity, resid, FUN = median), y = resid)) +
  geom_violin(aes(fill = clarity), show.legend = FALSE) +
  geom_boxplot(width = 0.08, fill = "lightblue") +
  labs(title = "Prices with the effect of diamond weight",
       x = "Category",
       y = "Variable") +
  coord_flip()

# Scatterplots by clarity
diamonds2 %>% ggplot(aes(x = log(carat), y = log(price), color = clarity)) +
  geom_point() +
  geom_smooth(method = "lm")

# Scatterplots by color
diamonds2 %>% ggplot(aes(x = log(carat), y = log(price), color = color)) +
  geom_point() +
  geom_smooth(method = "lm")

# Scatterplots by cut
diamonds2 %>% ggplot(aes(x = log(carat), y = log(price), color = cut)) +
  geom_point() +
  geom_smooth(method = "lm")

# Prediction Model

model <- lm(log(price) ~ log(carat) + clarity + color + cut + cut * log(carat),
            data = diamonds2)
model %>% summary()
model %>% anova()

# Predict price of diamonds
carat <- 1
clarity <- "IF"
color <- "J"
cut <- "Ideal"

explanatory <- data.frame(carat, cut, color, clarity)

predict(object = model, newdata = explanatory, interval = "predict") %>% exp()
