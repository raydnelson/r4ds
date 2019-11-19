# Model Basics
# Initial: 26 Mar 2019
# Revision: 26 Mar 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(forecast)
library(fImport)
library(Quandl)
library(timeSeries)
library(car)
library(magrittr)

# Simple linear regression
mtcars_lm <- lm(mpg ~ wt, data = mtcars)
mtcars_lm %>% str()
mtcars_lm %>% coefficients()
mtcars_lm %>% residuals()
mtcars_lm %>% fitted()

## How good is the model
mtcars_lm %>% 
  plot()
mtcars_lm %>% summary()

mtcars %>% 
  ggplot(aes(wt, mpg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(color = "red")

## Prediction with the linear model
mtcars_lm %>% predict(newdata = tibble(wt = 2))

### Function to predict
predict_cars <- function(x){
  mtcars_lm %>% predict(newdata = data.frame(wt = x))
}

predict_cars(5)
mtcars$wt %>% range()

# Loess Smooth (generalized additive model)

mtcars_loess <- loess(mpg ~ wt, data = mtcars)
mtcars_loess %>% str()
mtcars_loess %>% summary()
mtcars_loess %>% fitted()
mtcars_loess %>% residuals()

### Prediction with loess

predict(mtcars_loess, data.frame(wt = 2))

### prediction function for loess

predict_cars <- function(x){
  predict(mtcars_loess, data.frame(wt = x))
}

predict_cars(5)

# Prediction of retail sales

## Get retail sales data from FRED
retail <- fredSeries("RSAFSNA", from = "1992-01-01") %>% as.ts() %>% divide_by(1000)

## STL Decomposition

retail_stl <- retail %>%
  stl(s.window = 7)
retail_stl %>% 
  str()
retail_stl %>%
  autoplot() +
  labs("Retail sales have a pronounced trend and seasonality.")
retail_stl$
  

retail_stl %>% forecast(h = 12)
retail_stl %>% forecast(h = 12) %>% autoplot()

## ETS or exponential smoothing forecast

retail_ets <- retail %>%
  ets(model = "ZZZ")
retail_ets %>% str()
retail_ets %>% summary()
retail_ets %>% residuals()
retail_ets %>% fitted() 
retail_ets %>% 
  residuals() %>% 
  ggtsdisplay()

### Forecasting with ETS
retail_ets %>%
  forecast(h = 12)
retail_ets %>%
  forecast(h = 12) %>% 
  autoplot()

## ARIMA models

retail_arima <- retail %>% 
  auto.arima()
retail_arima %>% str()
retail_arima %>% summary()

### forecasting with ARIMA

retail_arima %>%
  forecast(h = 12)
retail_arima %>%
  forecast(h = 12) %>% 
  autoplot()

# Prediction of nonfarm employment
jobs <- Quandl('FRED/PAYNSA', type = 'ts') %>% divide_by(1000)

# Time Series Plot
jobs %>% ggtsdisplay()

## STL decomposition

jobs_stl <- jobs %>%
  stl(s.window = 7)
jobs_stl %>% 
  str()

jobs_stl %>%
  autoplot()
jobs_stl %>%
  forecast(h = 12)
jobs_stl %>%
  forecast(h = 12) %>% 
  autoplot()

## ETS forecast

jobs_ets <- jobs %>%
  forecast(h = 12)
jobs_ets %>% 
  summary()
jobs_ets %>% 
  autoplot()

## ARIMA forecast
jobs_arima <- jobs %>% 
  auto.arima()
jobs_arima %>%
  summary()
jobs_arima %>% 
  coefficients()

## AARIMA forecasts
jobs_arima %>% 
  forecast(h = 12)
jobs_arima %>% 
  forecast(h = 12) %>% 
  autoplot()

## Is there any information in the residuals?

jobs_arima %>% 
  residuals() %>% 
  ggtsdisplay()
