# R for Data Science Chapter 05
# Initial: May 17, 2018
# Revision: May 18, 2018
# Ray Nelson

# libraries
library(nycflights13)
library(tidyverse)

? flights
data(flights)
flights
flights %>% glimpse()
flights %>% View()

# Filter rows with filter()

(jan1 <- flights %>% filter(month == 1, day == 1))

sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1

near(sqrt(2) ^ 2, 2)
near( 1 / 49 * 49, 1)

filter(flights, month == 11 | month == 12)
filter(flights, month %in% 11:12)

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

# Missing Values

NA > 5
10 == NA
NA + 10
NA / 2
NA == NA

x <- NA
y <- NA
x == y
 is.na(x)
 
 df <- tibble(x = c(1, NA, 3))
filter(df, x > 1) 
filter(df, is.na(x) | x > 1)

# 5.2.4 Exercises

filter(flights, arr_delay >= 120)
filter(flights, dest %in% c("IAH", "HOU"))
filter(flights, carrier %in% c("UA", "AA", "DL"))
filter(flights, month %in% 7:9)
filter(flights, arr_delay >= 120 & dep_delay <=0)

