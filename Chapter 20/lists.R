library(tidyverse)

# Selection from a list
x <- c("one", "two", "three", "four", "five")
x[c(1, 2, 5)]
logical_selector <- x == "three"
x[logical_selector]

# Lists
figure <- mpg %>% 
  ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme(legend.position = "none")
figure
class(figure)
typeof(figure)

linear_model <- lm(hwy ~ displ, data = mpg)
class(linear_model)
typeof(linear_model)

logical <- TRUE
integer <- 10L
double <-  20
number <- list(integer = integer, double = double)
characters <- "Data Science"

class(figure)
is.list(figure)
class(linear_model)
is.list(linear_model)

ta_list <- list(laurel = "Laurel", moira = "Moira", sophie = "Sophie")

student_list <- list(jacob = "Jacob", lissie = "Lissie", kofi = "Kofi")

mpa634_list <- list(ta_list = ta_list, student_list = student_list)

master_list <- list(
                     slot_one = logical,
                     slot_two = integer,
                     slot_three = double,
                     slot_four = number,
                     slot_five = figure,
                     slot_six = linear_model,
                     slot_seven = mpa634_list,
                     slot_eight = mpg
                     )

master_list
master_list$slot_one
master_list$slot_two
master_list$slot_three
master_list$slot_four
master_list$slot_five
master_list$slot_six
master_list$slot_seven
master_list$slot_seven$ta_list
master_list$slot_seven$ta_list$moira
master_list$slot_seven$student_list
master_list$slot_seven$student_list$lissie
master_list$slot_eight

master_list$slot_seven$ta_list %>% as.character() %>% typeof()

master_list[7]
master_list[[7]]
master_list$slot_seven
master_list$slot_seven$moira

linear_model$fitted.values
linear_model %>% coefficients()
linear_model %>% summary()
summary(linear_model) %>% str()
summary(linear_model)$r.squared
summary(linear_model)
linear_model %>% summary() %>% coefficients()

# Creating factors
mpg$drv <- factor(mpg$drv,
                  levels = c("f", "r", "4"),
                  labels = c("front", "rear", "four-wheel"))

attributes(mpg$drv)
attr(mpg$drv, "levels")

attributes(mpg)

levels(mpg$drv)

mpg %>% 
  count(drv)

mpg %>% 
  ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Factor Example",
    color = "Type of Drive Train" 
  )

methods(autoplot)
methods(summary)
? autoplot
?autoplot

library(tidyverse)


diamond_cut <- diamonds %>% 
  pull(cut)

str(diamond_cut)

drive <- mpg %>% 
  pull(drv)

drive
str(drive)

drive <- factor(drive,
                levels = c("r", "4", "f"),
                labels = c("Rear", "Four", "Front"))

library(nycflights13)
library(Quandl)
library(fable)

destinations <- c("LAX", "ATL", "ORD")

flights %>% 
  filter(dest %in% destinations)

typeof(destinations)
class(destinations)

destinations <- flights %>% 
  count(dest, sort = TRUE) %>% 
  slice_head(n = 10) %>% 
  select(dest)

typeof(destinations)
class(destinations)

flights %>% 
  filter(dest %in% (destinations$dest))

flights %>% 
  semi_join(destinations)

str(destinations)
destinations$dest
destinations[1]
destinations[[1]]
destinations %>% pull()

retail <- Quandl('FRED/RSAFSNA',
          type = 'ts',
          start_date = '1959-01-01') %>% 
  as_tsibble()
class(retail)

retail %>% 
  autoplot()

retail_ets_model <- retail %>% 
  model(
    ets_model = ETS()
  )

class(retail)
class(retail_ets_model)

retail_forecast <- 
 retail_ets_model %>% 
  forecast(h = 12)

class(retail_forecast)

retail_forecast %>% 
  autoplot(retail) +
  labs(
    title = "title",
    subtitle = "subtitle",
    x = "x-axis",
    y = "y-axis",
    color = "color legend title",
    fill = "fill legend title",
    caption = "caption"
  )

retail

retail %>% 
  ggplot(aes(x = index %>% as.Date(), y = value)) +
  geom_line() +
  geom_smooth(span = 0.2)

retail$index %>% typeof()

retail %>% 
  typeof()

class(retail)
is_tibble(retail)

retail %>% 
  ggplot(aes(x = index %>% as.Date, y = value / 1000)) +
  geom_line() +
  geom_smooth(span = 0.25)


mpg$drv
mpg[7]
mpg[[7]]
mpg %>% pull(drv)

mpg$drv <- factor(mpg$drv,
                  level = c("r", "f", "4"),
                  labels = c("Rear", "Front", "Four")
                  )

str(mpg$drv)

mpg %>% 
  ggplot(aes(y = hwy, x = displ, color = drv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

logical_selector <- mpg$drv == "r"

drv <- mpg$drv
drv[logical_selector]
mpg[logical_selector, ]
