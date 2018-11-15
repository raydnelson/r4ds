# Model Building
# Initial: 15 November 2018
# Revision: 15 November 2018
# Ray Nelson

# Libraries --------------------------------------------------------------------
library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)
library(gridExtra)

# 24.2 Why are low quality diamonds more expensive? ----------------------------

diamonds %>% 
  ggplot(aes(x = cut, y = price)) +
    geom_boxplot()
diamonds %>% 
  ggplot(aes(x = color, y = price)) +
  geom_boxplot()
diamonds %>% 
  ggplot(aes(x = clarity, y = price)) +
  geom_boxplot()

## 24.2.1 Price and carat
ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)

diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) + 
  geom_hex(bins = 50)

diamonds2 %>% 
  ggplot(aes(cut, lresid)) +
  geom_ref_line(h = 0, colour = "grey70", size = 1.25) +
    geom_boxplot()

diamonds2 %>%
  ggplot(aes(color, lresid)) +
    geom_ref_line(h = 0, colour = "grey70", size = 1.25) +
    geom_boxplot()

diamonds2 %>%
  ggplot(aes(clarity, lresid)) +
    geom_ref_line(h = 0, colour = "grey70", size = 1.25) +
    geom_boxplot()

# 24.2.2 A more complicated model
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamond2) %>% 
  add_predictions(mod_diamond2)
grid

grid %>% ggplot(aes(x = cut, y = pred)) + 
  geom_point()

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

diamonds2 %>% ggplot(aes(lcarat, lresid2)) + 
  geom_hex(bins = 50)

diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)

# 24.3 What affects the number of daily flights? -------------------------------

daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())
daily

daily %>% ggplot(aes(date, n)) + 
  geom_line()

# 24.3.1 Day of week

daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))
daily %>%
  ggplot(aes(x = wday, y = n, fill = wday)) + 
    geom_boxplot(show.legend = FALSE, alpha = 1/3)

mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

daily %>% ggplot(aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4)

daily <- daily %>% 
  add_residuals(mod)
daily %>% 
  ggplot(aes(date, resid)) + 
    geom_ref_line(h = 0) + 
    geom_line()

daily %>% 
  ggplot(aes(date, resid, colour = wday)) + 
    geom_ref_line(h = 0) + 
    geom_line()

daily %>% 
  filter(resid < -100)

daily %>% 
  ggplot(aes(date, resid)) + 
    geom_ref_line(h = 0) + 
    geom_line(colour = "grey50") + 
    geom_smooth(se = FALSE, span = 0.20)

## 24.3.2 Sesonal Saturday effect

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) + 
    geom_point() + 
    geom_line() +
    scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

daily <- daily %>% 
  mutate(term = term(date)) 

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
    geom_point(alpha = 1/3) + 
    geom_line() +
    scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

daily %>% 
  ggplot(aes(wday, n, colour = term)) +
    geom_boxplot()

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
    geom_line(alpha = 0.75)

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() + 
  geom_point(data = grid, colour = "red") + 
  facet_wrap(~ term)

mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) + 
    geom_ref_line(h = 0, size = 2, colour = "white") + 
    geom_line()

# 24.3.3 Computed variables

compute_vars <- function(data) {
  data %>% 
    mutate(
      term = term(date), 
      wday = wday(date, label = TRUE)
    )
}

wday2 <- function(x) wday(x, label = TRUE)
mod3 <- lm(n ~ wday2(date) * term(date), data = daily)

# 24.3.4 Time of year: an alternative approach

library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
    geom_line() +
    geom_point()