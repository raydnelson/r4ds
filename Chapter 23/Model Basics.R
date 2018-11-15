# Model basics
# Initial: 14 November 2018
# Revision: 14 November 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(modelr)
library(gridExtra)
options(na.action = na.warn)


# 23.2 A Simple model ----------------------------------------------------------
sim1 %>%
  ggplot(aes(x = x, y = y)) +
    geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x = x , y = y)) +
  geom_abline(data = models, aes(intercept = a1, slope = a2), alpha = 1/4) +
  geom_point()

model1 <- function(a, data){
  a[1] + data$x * a[2]
}

model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data){
  diff <- data$y -model1(mod, data)
  sqrt(mean(diff^2))
}

measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2){
  measure_distance(c(a1, a2), sim1)
}

(models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist)))

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

# 23.3 Visualising models ------------------------------------------------------

(grid <- sim1 %>% 
   data_grid(x))

(grid <- grid %>% 
    add_predictions(sim1_mod))

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

(sim1 <- sim1 %>% 
  add_residuals(sim1_mod))

residuals_freqpoly <- sim1 %>%
  ggplot(aes(x = resid)) +
    geom_freqpoly(binwidth = 0.5, fill = "lightgreen")

residuals_density <- sim1 %>%
  ggplot(aes(x = resid)) +
  geom_density(adjust = 1/2, fill = "lightblue")


grid.arrange(residuals_freqpoly, residuals_density, nrow = 2)


sim1 %>% 
  ggplot(aes(x = x, y = resid)) +
    geom_ref_line(h = 0) +
    geom_point()

# 23.4 Formulas and model families

(df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
))

model_matrix(df, y ~ x1)

model_matrix(df, y ~ x1 -1)

model_matrix(df, y ~ x1 + x2)

## 23.4.1 Categorical variables

(df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
))

model_matrix(df, response ~ sex)

sim2 %>% 
  ggplot(aes(x = x, y = y)) +
    geom_point()

mod2 <- lm(y ~ x, data = sim2)

(grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2))

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)


tibble(x = "e") %>% 
  add_predictions(mod2)

# 23.4.2 Interactions (continuous and categorical)

sim3 %>% ggplot(aes(x = x1, y = y)) +
  geom_point(aes(colour = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

sim3 %>% 
  data_grid(x1, x2)

(grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2))

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(rows = vars(model), cols = vars(x2))

head(sim3)

# 23.4.3 Interactions (two continuous)

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid

seq_range(c(0.0123, 0.923423), n = 5)
seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)

x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10)
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.50)

x2 <- c(0, 1)
seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.10)
seq_range(x2, n = 5, expand = 0.25)
seq_range(x2, n = 5, expand = 0.50)

ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

# 23.4.4 Transformations

df <- tribble(
  ~y, ~x,
  1, 1,
  2, 2,
  3, 3
)

df %>% model_matrix(y ~ x^2 + x)
df %>% model_matrix(y ~ I(x^2) + x)
df %>% model_matrix(y ~ poly(x, 2))

library(splines)
df %>% model_matrix(y ~ ns(x, 2))

sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

sim5 %>%
  ggplot(aes(x, y)) +
    geom_point()

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)

# 23.5 Missing values

df <- tribble(
  ~x, ~y,
  1, 2.2,
  2, NA,
  3, 3.5,
  4, 8.3,
  NA, 10
)

mod <- lm(y ~ x, data = df)

mod <- lm(y ~ x, data = df, na.action = na.exclude)
nobs(mod)          
