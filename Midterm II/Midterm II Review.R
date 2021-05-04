library(tidyverse)
library(nycflights13)

data(mpg)
data(diamonds)

cylinders <- mpg$cyl

mpg %>% 
  mutate(cyl = factor(cyl, levels = c(5,4,6,8), labels = c("four", "five", "six", "eight"))) %>% 
  ggplot(aes(y = cyl)) +
  geom_bar()

cylinders_nominal <- cylinders %>%
  factor(levels = c(4,5,6,8), labels = c("four", "five", "six", "eight"))

cylinders_ordinal <- cylinders %>%
  factor(levels = c(4,5,6,8), labels = c("four", "five", "six", "eight"),
         ordered = TRUE)
cylinders_ordinal

diamonds_price <- diamonds %>% 
  select(price) %>% 
  slice_sample(n = 1000)

diamonds_price %>%
  mutate(z_score = scale(price),
         abs_z_score = abs(z_score))  %>% 
  filter(abs_z_score >= 3)

diamonds_price %>% 
  mutate(z_score = (price - mean(price)) / sd(price)) %>% 
  filter(abs(z_score) >= 3)

(destinations <- flights %>% 
  filter(origin == "JFK") %>% 
  distinct(dest)
)

flights %>% 
  filter(dest %in% destinations)

flights %>% 
  semi_join(destinations)

top_ten <- flights %>% 
  group_by(dest) %>% 
  count(dest) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  slice_head(n = 10) %>% 
  pull(dest)

flights %>% 
  filter(dest %in% top_ten)

top_ten <- flights %>% 
  group_by(dest) %>% 
  count(dest) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  slice_head(n = 10)

flights %>% 
  anti_join(top_ten)

top_ten %>% 
  left_join(airports, by = c("dest" = "faa")) %>% 
  select(dest, name, lat, lon)

flights %>% 
  distinct(dest)

airports$faa

(income <- rnorm(n = 100, mean = 10000, sd = 5000))

income %>% 
  cut(breaks = c(-Inf, 5000, 10000, +Inf), labels = c("low", "medium"," high"))

if(income < 5000){
  "low"
} else if (income < 10000){
  "middle"
} else {
  "high"
}

switch(1, "Tanner", "Kristen")

person <-  1

if(identical(person, 1)){
  "Tanner" 
} else {
  "Kristen"
}


