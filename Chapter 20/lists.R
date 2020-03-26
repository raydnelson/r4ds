library(tidyverse)

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

MPA634_list <- list(cecily = "Cecily", andrew = "Andrew", moira = "Moira")

storage_list <- list(
                     slot_one = logical,
                     slot_two = integer,
                     slot_three = double,
                     slot_four = number,
                     slot_five = figure,
                     slot_six = linear_model,
                     slot_seven = MPA634_list,
                     slot_eight = mpg
                     )

storage_list$slot_one
storage_list$slot_two
storage_list$slot_three
storage_list$slot_four
storage_list$slot_five
storage_list$slot_six
storage_list$slot_seven
storage_list$slot_eight

storage_list[7]
storage_list[[7]]
storage_list$slot_seven
storage_list$slot_seven$moira

linear_model$fitted.values
linear_model %>% coefficients()
linear_model %>% summary()
summary(linear_model) %>% str()
summary(linear_model)$r.squared
linear_model %>% summary() %>% coefficients()

# Creating factors
mpg %>% glimpse()

mpg$drv <- factor(mpg$drv,
                  levels = c("f", "r", "4"),
                  labels = c("front", "rear", "four-wheel"))

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

