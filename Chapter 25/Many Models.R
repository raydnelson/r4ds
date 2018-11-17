# Chapter 25: Many models
# Initial: 16 November 2018
# Revision: 16 November 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(modelr)
library(gapminder)
library(gridExtra)
library(broom)

# 25.2 gapminder ---------------------------------------------------------------
gapminder

gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
    geom_line()

nz <- gapminder %>% 
  filter(country == "New Zealand")

full <- nz %>% 
  ggplot(aes(x = year, y = lifeExp)) +
    geom_line() +
    ggtitle("Full data = ")
nz_model <- lm(lifeExp ~ year, data = nz)

linear <- nz %>% 
  add_predictions(nz_model) %>% 
  ggplot(aes(x = year, y = pred)) +
    geom_line() +
    ggtitle("Linear Trend +")

residual <- nz %>% 
  add_residuals(nz_model) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")

grid.arrange(full, linear, residual, nrow = 1)

# 25.2.1 Nested data

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country %>% str()
by_country$data[[1]]

# 25.2.2 List-columns
country_model <- function(df){
  lm(lifeExp ~ year, data = df)
}

models <- map(by_country$data, country_model)
  
by_country <- by_country %>%
  mutate(model = map(data, country_model))
by_country

by_country %>% 
  filter(continent == "Europe")

by_country %>% 
  arrange(continent, country)

# 25.2.3 Unnesting
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country

resids <- unnest(by_country, resids)
resids

resids %>% 
  ggplot(aes(year, resid)) +
    geom_line(aes(group = country), alpha = 1 / 3) + 
    geom_smooth(se = FALSE)

resids %>% 
  ggplot(aes(year, resid, group = country)) +
    geom_line(alpha = 1 / 3) + 
    facet_wrap(~continent)

# 25.2.4 Model quality

broom::glance(nz_model)

by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance

glance %>% 
  arrange(r.squared)

glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
    geom_line()

# 25.3 List-columns ------------------------------------------------------------

data.frame(x = list(1:3, 3:5))
data.frame(
  x = I(list(1:3, 3:5)), 
  y = c("1, 2", "3, 4, 5")
)
tibble(
  x = list(1:3, 3:5), 
  y = c("1, 2", "3, 4, 5")
)

tribble(
  ~x, ~y,
  1:3, "1, 2",
  3:5, "3, 4, 5"
)

# 25.4 Creating list-columns ---------------------------------------------------

## 25.4.1 With nesting

gapminder %>% 
  group_by(country, continent) %>% 
  nest()

gapminder %>% colnames()
gapminder %>% 
  nest(year:gdpPercap)

## 25.4.2 From vectorized functions

df <- tribble(
  ~x1,
  "a,b,c", 
  "d,e,f,g"
) 

df %>% 
  mutate(x2 = stringr::str_split(x1, ","))

df %>% 
  mutate(x2 = stringr::str_split(x1, ",")) %>% 
  unnest()

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>%
  mutate(sims = invoke_map(f, params, n = 10))

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>%
  mutate(sims = invoke_map(f, params, n = 10))

# 25.4.3 From multivalued summaries
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = quantile(mpg))

mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg)))

probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>% 
  group_by(cyl) %>% 
  summarise(p = list(probs), q = list(quantile(mpg, probs))) %>% 
  unnest()

# 25.4.4 From a named list
x <- list(
  a = 1:5,
  b = 3:4, 
  c = 5:6
)
x

df <- enframe(x)
df

df %>% 
  mutate(
    smry = map2_chr(name, value, ~ stringr::str_c(.x, ": ", .y[1]))
  )

# 25.5 Simplifying list-columns
## 25.5.1 List to vector

df <- tribble(
  ~x,
  letters[1:5],
  1:3,
  runif(5)
)

df %>% mutate(
  type = map_chr(x, typeof),
  length = map_int(x, length)
)

# 25.5.2 Unnesting

tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y)

df1 <- tribble(
  ~x, ~y,           ~z,
  1, c("a", "b"), 1:2,
  2, "c",           3
)

df1 %>% unnest(y, z)

df2 <- tribble(
  ~x, ~y,           ~z,
  1, "a",         1:2,  
  2, c("b", "c"),   3
)
df2

df2 %>% unnest(y, z)
df1
