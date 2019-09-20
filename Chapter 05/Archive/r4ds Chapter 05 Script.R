# R for Data Science Chapter 05
# Initial: May 17, 2018
# Revision: September 19, 2018
# Ray Nelson


# 5.1 Introduction --------------------------------------------------------

# libraries
library(nycflights13)
library(tidyverse)

# Information about flights

? flights
data(flights)
flights
glimpse(flights)
View(flights)

# 5.2 Filter rows with filter() -------------------------------------------

jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))

# Comparisons

filter(flights, month = 1)
filter(flights, month == 1)

sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1

near(sqrt(2) ^ 2, 2)
near( 1 / 49 * 49, 1)

# Logical operators

filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))

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

# 1 Find all flights that
# a.  Had an arrival delay of two or more hours
filter(flights, arr_delay >= 120)

# b.  Flew to Houston (IAH or HOU)
filter(flights, dest %in% c("IAH", "HOU"))

# c.  Were operated by United, American, or Delta
filter(flights, carrier %in% c("UA", "AA", "DL"))

# d.  Departed in summer (July, August, or September)
filter(flights, month %in% 7:9)

# e.  Arrived more than two hours late, but didn't leave late
filter(flights, !is.na(dep_delay), arr_delay > 120, dep_delay <= 0)

# f.  Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)

# h.  Departed between midnight and 6am (inclusive)
filter(flights, dep_time <= 600 | dep_time == 2400)

# 2 the between function
filter(flights, between(month, 7, 9))

# 3 Missing departure time
filter(flights, is.na(dep_time))

# Missing
NA ^ 0
NA | TRUE
FALSE & NA
NA * 0

# 5.3 Arrange rows with arrange() -----------------------------------------

arrange(flights, year, month, day)

df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

# 5.3.1 Exercises

#1  How could you use arrange() to sort all missing values to the start?
# (Hint: use is.na()).
arrange(flights, desc(is.na(dep_delay)), dep_delay)

#2  Sort flights to find the most delayed flights.
#   Find the flights that left earliest.
arrange(flights, desc(dep_delay))
arrange(flights, dep_delay)

#3  Sort flights to find the fastest flights
arrange(flights, air_time)

#4  Which flights travelled the longest? Which travelled the shortest?
arrange(flights, desc(distance)) %>% select(distance, everything())
arrange(flights, distance) %>% select(distance, everything())


# 5.4  Select columns with select -----------------------------------------

select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

select(flights, tail_num = tailnum)
rename(flights, tail_num = tailnum)

select(flights, time_hour, air_time, everything())

# 5.4.1 Exercises

#1  Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time,
#   and arr_delay from flights.
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, starts_with("dep"), starts_with("arr"))
select(flights, matches(flights, "^(dep|arr)_(time|delay)$"))

#2  What happens if you include the name of a variable multiple times
#   in a select() call?
select(flights, year, month, day, year)

#3  What does the one_of() function do? Why might it be helpful in conjunction
#   with this vector?
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, vars)
select(flights, vars %>% one_of())

# 4 Does the result of running the following code surprise you?
#   How do the select helpers deal with case by default?
#   How can you change that default?
select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = FALSE))

# 5.5 Add new variables with mutate() -------------------------------------

flights_sml <- select(
  flights, 
  year:day,
  ends_with("delay"),
  distance,
  air_time
)

mutate(
  flights_sml, 
  gain = arr_delay -dep_delay,
  speed = distance / air_time * 60
)

mutate(
  flights_sml, 
  gain = arr_delay -dep_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)

transmute(flights, 
  gain = arr_delay - dep_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)

# 5.5.1 Useful creation functions

transmute(flights, 
  dep_time,
  hour = dep_time %/% 100,
  minute = dep_time %% 100
)

(x <- 1:10)
lag(x)
lead(x)
cumsum(x)
cummean(x)

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))

row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

# 5.5.2 Exercises

# 1 Currently dep_time and sched_dep_time are convenient to look at,
#   but hard to compute with because they’re not really continuous numbers.
#   Convert them to a more convenient representation of number of minutes
#   since midnight.

flights %>%
  select(dep_time, sched_dep_time) %>% 
  mutate(
    dep_time_minutes = dep_time %/% 100 * 60 + dep_time %% 100,
    sched_dep_time_minutes = sched_dep_time %/% 100 * 60 + sched_dep_time %% 100
    )

time2mins <- function(x){
  x %/% 100 * 60 + x %% 100
}

flights %>% select(dep_time, sched_dep_time) %>% 
  mutate(
    dep_time_mins = time2mins(dep_time),
    sched_dep_time_mins = time2mins(sched_dep_time)
    )

#2  Compare air_time with arr_time - dep_time. What do you expect to see?
#   What do you see? What do you need to do to fix it?
flights %>%
  select(air_time, arr_time, dep_time)

#3  Compare dep_time, sched_dep_time, and dep_delay. How would you expect
#   those three numbers to be related?
flights %>% select(dep_time, sched_dep_time, dep_delay) %>% 
  mutate(dep_delay_new = dep_time %>% time2mins() - sched_dep_time %>% time2mins())

#4  Find the 10 most delayed flights using a ranking function.
#   How do you want to handle ties? Carefully read the documentation
#   for min_rank().
flights %>%
  mutate(ranking = -dep_delay %>% 
           min_rank()) %>% 
  arrange(ranking) %>% 
  select(ranking, dep_time, sched_dep_time, dep_delay) %>% 
  filter(ranking <= 10)

flights %>%
  mutate(ranking = dep_delay %>% min_rank()) %>% 
  arrange(ranking %>% desc()) %>%
  select(ranking, dep_time, sched_dep_time, dep_delay) %>% 
  filter(row_number() <= 10)

# 5.  What does 1:3 + 1:10 return? Why?
1:3 + 1:10

# 6
? Trig

# 5.6 Grouped summaries with summarise ----------------------------------

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

flights %>%
  group_by(year, month, day) %>% 
  summarise(delay = dep_delay %>% mean(na.rm = TRUE))

# 5.6.1 Multiple operations with pipes
## No pipes
by_dest <- group_by(flights, dest)
by_dest

delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
                   )
delay

delay <- filter(delay, count > 20, dest != "HNL")
delay

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3)

## With pipes

flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = distance %>% mean(na.rm = TRUE),
    delay = arr_delay %>% mean(na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL") %>%
  ggplot(mapping = aes(x = dist, y = delay)) +
    geom_point(aes(size = count), alpha = 1 / 3) +
    labs(
      title = "Flight Delays versus Flight Distance",
      subtitle = "Data for 2013 Flights",
      x = "Average Distance",
      y = "Average Delay",
      size = "Number of Flights"
  )

# Missing Values

flights %>% group_by(year, month, day) %>% 
  summarise(mean = dep_delay %>% mean())

flights %>% group_by(year, month, day) %>% 
  summarise(mean = dep_delay %>% mean(na.rm = TRUE))

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = dep_delay %>% mean())

# Counts

# Airline delays
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = arr_delay %>% mean
    )

ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(bindwith = 10)

delays %>% ggplot(mapping = aes(x = delay)) +
  geom_density(fill = "lightblue")

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
    geom_point(alpha = 1/10)

# Batting Averages

batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ab = sum(AB, na.rm = TRUE),
    ba = sum(H, na.rm = TRUE) / ab
  )

batters %>% filter (ab > 5000) %>% arrange(ba %>% desc())

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
    geom_point(alpha = 1/10) + 
    geom_smooth(se = FALSE)

batters %>% 
  arrange(desc(ba))

# 5.6.4 Useful Summary Functions

## Measures of location

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

## Measures of spread

# Why is distance to some destinations more variable than to others?

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

## Measures of rank

# When do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  ) %>% 
  arrange(first, last)

## Measures of position

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = dep_time %>% desc() %>% min_rank()) %>% 
  filter(r %in% range(r)) %>% 
  select(year:day, dep_time, r)

# Counts

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carriers)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  count(tailnum, wt = distance)

# Counts and proportions of logical values

# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))

# Grouping by multiple variables

daily <- flights %>% group_by(year, month, day)
(per_day <- daily %>% summarise(flights = n()))

(per_month <- per_day %>% summarise(flights = sum(flights)))

(per_year <- per_month %>% summarise(flights = sum(flights)))

# 5.6.6 Ungrouping

daily %>% group_by(year, month) %>% 
  summarise(flights = n())

daily %>% ungroup() %>% 
  summarise(flights = n())

# 5.6.7 Exercises

# 1. Brainstorm at least 5 different ways to assess the typical delay
#   characteristics of a group of flights. Consider the following scenarios:

# a.  A flight is 15 minutes early 50% of the time, and 15 minutes late 50% 
#     of the time

# b.  A flight is always 10 minutes late.

# c.  A flight is 30 minutes early 50% of the time, and 30 minutes late 50%
#     of the time.

# d.  99% of the time a flight is on time. 1% of the time it’s 2 hours late.


# 5.7 Grouped mutates (and filters) ---------------------------------------

flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

# 5.7.1 Exercises

#1. Refer back to the lists of useful mutate and filtering functions. 
#   Describe how each operation changes when you combine it with grouping.

#2. Which plane (tailnum) has the worst on-time record?
  
#3. What time of day should you fly if you want to avoid delays as much as possible?
  
#4.  For each destination, compute the total minutes of delay. For each, flight, 
#   compute the proportion of the total delay for its destination.

#5. Delays are typically temporally correlated: even once the problem that caused
#   the initial delay has been resolved, later flights are delayed to allow
#   earlier flights to leave. Using lag() explore how the delay of a flight is
#   related to the delay of the immediately preceding flight.

#6. Look at each destination. Can you find flights that are suspiciously fast?
#   (i.e. flights that represent a potential data entry error). Compute the 
#   air time a flight relative to the shortest flight to that destination. 
#   Which flights were most delayed in the air?
  
#7. Find all destinations that are flown by at least two carriers. Use that
#   information to rank the carriers.

#8. For each plane, count the number of flights before the first delay of
#   greater than 1 hour.
