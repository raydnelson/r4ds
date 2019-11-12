library(tidyverse)
library(lubridate)
library(nycflights13)


mpg %>%
  select(hwy, cty) %>% 
  gather(key = "type_of_driving", value = "MPG") %>% 
  group_by(type_of_driving) %>% 
  summarise(Mean = mean(MPG, na.rm = TRUE),
            Standard_Deviation = sd(MPG, na.rm = TRUE))

flights %>%
  inner_join(airports, by = c("origin" = "faa")) %>% 
  inner_join(airports, by = c("dest" = "faa")) %>% 
  inner_join(airlines, by = carrier)

mpg %>%
  mutate(drv = drv %>% fct_infreq() %>% fct_rev()) %>% 
  ggplot(aes(drv)) +
  geom_bar()

flights %>%
  select(month, day, dep_time) %>% 
  mutate(Date = make_datetime(2013, month, day, dep_time %/% 100, dep_time %% 100))

mpg$mileage <- mpg$hwy %>% 
  cut(breaks = c(10, 18, 25, 45), labels = c("Low", "Medium", "High"))
mpg %>% 
  count(mileage)

# Problem #1
# Number of months in a mortgage function
calc_mortgage <- function(begin_month, end_month) {
  (ymd(begin_month) %--% ymd(end_month)) %/% months(1)
}

calc_mortgage(20190331, 20340331)

# Problem #2
# Lists

mpg_lm <- lm(hwy ~ displ, data = mpg)
mpg_lm %>% str()

mpg_residuals <- mpg_lm$residuals
mpg_fitted <- mpg_lm$fitted.values

(fitted_residuals <- tibble(mpg_fitted, mpg_residuals))
  
mpg_residuals <- mpg_lm[[2]]
mpg_fitted <- mpg_lm[[5]]

(fitted_residuals <- tibble(mpg_fitted, mpg_residuals))

# Problem #3 Factor Recode
# Factor recode
mpg %>% 
  count(drv)
mpg$drv <- fct_recode(mpg$drv,
                      "front" = "f",
                      "four" = "4",
                      "rear" = "r")
mpg %>% 
  count(drv)

# Problem #4 Relational Data
# Flights

flights_jfk <- flights %>% 
  filter(origin == "JFK")

flights_jfk_top <- flights_jfk %>%
  count(carrier, sort = TRUE) %>% 
  head(5)

flights_jfk %>%
  semi_join(flights_jfk_top)

# Problem #5
# Outlier Function

outliers <- function(x, mean_x, sd_x) {
  z <- (x - mean_x) / sd_x
  z %>% print()
  if(z < -3 || z > 3) {
    "possible outlier"
  } else {
    "not outlier"
  }
}

outliers(4, 0, 1)
outliers(4, 10, 10)
outliers(8, 1, 2)
  
