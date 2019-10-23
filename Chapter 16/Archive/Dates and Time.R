# Dates and Time
# Initial: 26 Oct 2018
# Revision: 5 Nov 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(lubridate)
library(nycflights13)

# 16.2 Creating date/times -----------------------------------------------------

# Current date and date-time
today()
now()

# 16.2.1 From Strings
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

ymd(20170131)
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

ymd(20170131, tz = "UTC")

# 16.2.2 From individual components
flights %>%
  select(year, month, day, hour, minute) 

flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

flights %>% 
  select(dep_time)

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

make_datetime_100(year = 2018, month = 10, day = 29, time = 1431)

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt
flights_dt %>% glimpse()

flights_dt %>% 
  ggplot(aes(dep_time)) + 
    geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(x = dep_time)) +
    geom_freqpoly(binwidth = 600) # 600 seconds = 10 minutes


# 16.2.3 From other types

now() %>% as_datetime()
now() %>% as_date()

# Unix Epoch, 1970-01-01

as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)

# 16.3 Date-time compoonents ---------------------------------------------------

## 16.3.1 Getting components

(datetime <- ymd_hms("2016-07-08 12:34:56"))

year(datetime)
month(datetime)
mday(datetime)

yday(datetime)
wday(datetime)

month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday, fill = wday)) +
    geom_bar(show.legend = FALSE) +
    scale_fill_brewer(palette = "Set3")

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) %>% 
  ggplot(aes(x = minute, y = avg_delay)) +
    geom_line(size = 1,  color = "blue")

sched_dep <- flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE),
            n = n()
            )

sched_dep %>%
  ggplot(aes(x = minute, y = avg_delay)) +
  geom_line()

sched_dep %>% 
  ggplot(aes(x = minute, n)) +
    geom_line()

# 16.3.2 Rounding

flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
    geom_line()

next_year <- today() + years(1)

(next_year %--% today()) %/% days(1)

# 16.3.3 Setting components
(datetime <- ymd_hms("2016-07-08 12:34:56"))

year(datetime) <- 2020
datetime

month(datetime) <- 01
datetime

today() %--% next_year

(ymd(19510414) %--% today()) / years(1)

hour(datetime) <- hour(datetime) + 1
datetime

update(datetime, year = 2020, month = 2, mday = 2, hour = 2, minutes = 2, seconds = 2)

ymd("2015-02-01") %>% 
  update(mday = 30)

ymd("2015-02-01") %>% 
  update(hour = 400)

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  ggplot(aes(x = dep_hour)) +
    geom_freqpoly(binwidth = 300) # Five minute intervals

# Time spans -------------------------------------------------------------------

# 16.4.1 Durations
# How old is Hadley
(age <- today() - ymd(19791014))

as.duration(age)

dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)

2 * dyears(1)

dyears(1) + dweeks(12) + dhours(15)

(tomorrow <- today() + ddays(1))

(last_year <- today() - dyears(1))

(one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York"))

(one_pm + ddays(1))

# 16.4.2 Periods

one_pm
one_pm + days(1)

seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
years(1)

# Arithmetic with periods
10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

# A leap year
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

# Daylight Savings Time
one_pm + ddays(1)

one_pm + days(1)

# Periods and flight dates

flights_dt %>% 
  filter(arr_time < dep_time)

flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )

flights_dt %>% 
  filter(arr_time < dep_time)

# 16.4.3 Intervals
years(1) / days(1)
(next_year <- today() + years(1))
(today() %--% next_year) / ddays(1)


# 16.5 Time zones --------------------------------------------------------------

Sys.timezone()

length(OlsonNames())

OlsonNames() %>% head()

(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))

x1 - x2
x1 - x3

(x4 <- c(x1, x2, x3))

(x4a <- with_tz(x4, tzone = "Australia/Lord_Howe"))

x4a - x4

(x4b <- force_tz(x4, tzone = "Australia/Lord_Howe"))

x4b - x4


# Exercises

d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)
