# Conditional Execution
# Initial: 8 Mar 2019
# Revision: 8 Mar 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(nycflights13)

# Attendance or regular quiz function

choose_quiz <- function(probability_attendance){
  if (runif(1) < probability_attendance) "Attendance" else "Regular"
}

# Test the function

choose_quiz(.5)

# Simulation to see if the function is fair

outcomes <- vector()
for (i in 1:10000){
  outcomes[i] <- choose_quiz(.6)
}

outcomes %>% table()

tibble(number = outcomes) %>% 
  count(number)

# Choose the probability of an attendance based on number of days since last attendance quiz

probability_attendance <- function(days_since_attendance) {
  if (days_since_attendance == 0) {
    probability_attendance <- 0.30
  } else if (days_since_attendance == 1) {
    probability_attendance <- 0.40
  } else if (days_since_attendance == 2) {
    probability_attendance <- 0.50
  } else if (days_since_attendance == 3) {
    probability_attendance <- 0.60
  } else if (days_since_attendance == 4) {
    probability_attendance <- 0.70
  } else {
    probability_attendance <- 0.80
  }
  return(probability_attendance)
}

# Test the Function
probability_attendance(3)

# Simplier version of the function

probability_attendance <- function(days_since_attendance) {
  if (days_since_attendance > 5)
    days_since_attendance <- 5
  switch(days_since_attendance + 1,
         0.30,
         0.40,
         0.50,
         0.60,
         0.70,
         0.80)
}

# Test the Function
probability_attendance(3)

# Function that chooses the quiz
attendance_or_regular <- function(days_since_attendance) {
  probability <- probability_attendance(days_since_attendance)
  paste("The probability of an attendance quiz:", probability) %>%
    print()
  quiz <- choose_quiz(probability)
  paste("Today's quiz is: ", quiz)
}

attendance_or_regular(0)


# Rock, paper, scissors

choose_object <- function(){
  x <- runif(1)
  if (x < 1/3){
    game <- "rock"
  } else if (x > 2/3){
    game <- "scissors"
  } else {
    game <- "paper"
  }
}

select_winner <- function(person1, person2) {
  if (person1 == "rock"){
    if (person2 == "rock"){
      outcome = "ties"
    } else if (person2 == "paper") {
      outcome = "loses"
    } else {
      outcome = "wins"
    }
  } else if (person1 == "paper") {
      if (person2 == "rock"){
        outcome = "wins"
      } else if (person2 == "paper") {
        outcome = "ties"
      } else {
        outcome = "loses"
      }
  } else {
      if (person2 == "rock"){
        outcome = "loses"
      } else if (person2 == "paper") {
        outcome = "wins"
      } else {
        outcome = "ties"
      }
  }
  paste0("Person 1 ", outcome, ".")
}

play_game <- function() {
  person1 <- choose_object()
  paste0("Person 1 chose: ", person1) %>% print()
  person2 <- choose_object()
  paste0("Person 2 chose: ", person2) %>% print()
  select_winner(person1, person2)
}

play_game()

# Below average and above average highway mileage using cut and ifelse

(intervals <- c(0, mean(mpg$hwy), max(mpg$hwy)))
cut(mpg$hwy, intervals, labels = c("below average", "above average"))

ifelse (mpg$hwy < mean(mpg$hwy), "below average", "above average")


# Function that determines whether a temperature is below, at, or above freqqzing

freezing <- function(temperature){
  if(temperature < 32) "below"
  else if (temperature == 32) "freezing"
  else "above"
}

freezing(24)

# function that labels a flight as early, on time, or late from the flights data

status <- function(delay){
  test <- vector()
  test[is.na(delay)] <- "canceled"
  test[delay < 0] <- "early"
  test[delay == 0] <- "ontime"
  test[delay > 0] <- "late"
  return(test)
}

flights$arr_delay %>% 
  as_vector() %>% 
  status() %>% 
  table()

test <- flights$arr_delay
test[is.na(flights$arr_delay)] <- "cancelled"
test[flights$arr_delay < 0] <- "early"
test[flights$arr_delay == 0] <- "ontime"
test[flights$arr_delay > 0] <- "late"
table(test)

ontime(flights$arr_delay)

flights %>%
  mutate(arrival_status = ) %>%
  count(arrival_status)


cut(flights$arr_delay, c(-Inf, 0,  +Inf), c("early", "late"))
