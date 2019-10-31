library(tidyverse)
library(lubridate)

greet_now <- function() {
  Time <- now() %>% lubridate::hour()
  if(Time < 12){
    "Good morning"
  } else if (Time < 17){
    "Good afternoon"
  } else {
    "Good evening"
  }
}

greet_now()

greet_time <- function(time){
  Time <- time %>% lubridate::hour()
  if(Time < 12){
    "Good morning"
  } else if (Time < 17){
    "Good afternoon"
  } else {
    "Good evening"
  }
}

greet_time(ymd_h("2019-03-11:11"))


fizzbuzz <- function(number) {
  if(number %% 3 == 0 && number %% 5 == 0){
    "fizzbuzz"
  } else if (number %% 3 == 0){
    "fizz"
  } else if (number %% 5 == 0){
    "buzz"
  } else {
    number
  }
}

fizzbuzz(13)
