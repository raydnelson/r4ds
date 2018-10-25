# Factors
# Initial: October 25, 2018
# Revision: October 25, 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(forcats)

# 15.2 Creating factors
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1

y2 <- factor(x2, levels = month_levels)
y2

factor(x1)

f1 <- factor(x1, levels = unique(x1))
unique(x1)



f2 <- x1 %>% factor() %>% fct_inorder()
f2

f2 %>% levels()

# 15.3 General Social Survey
gss_cat
gss_cat %>% glimpse()

gss_cat %>% 
  count(race)

gss_cat %>% 
  ggplot(aes(x = race, fill = race)) +
    geom_bar(show.legend = FALSE)

gss_cat %>% 
  ggplot(aes(x = race, fill = race)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(drop = FALSE)

# 15.3.1 Exercise

summary(gss_cat$rincome)
gss_cat %>% 
  ggplot(aes(x = rincome, fill = rincome)) +
    geom_bar()

gss_cat %>% 
  ggplot(aes(x = rincome, fill = rincome)) +
  geom_bar(show.legend = FALSE) +
  coord_flip()

# 15.4 Modifying factor order

## TV Hours by religion
relig_summary <- gss_cat %>% 
  group_by(relig) %>% 
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

relig_summary %>% 
  ggplot(aes(tvhours, relig)) +
    geom_point()

relig_summary %>% 
  ggplot(aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

relig_summary %>% 
  mutate(relig = fct_reorder(relig, tvhours)) %>% 
  ggplot(aes(tvhours, relig)) +
    geom_point()

## Average age over reported income levels

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    n = n()
  )

rincome_summary %>%
  mutate(rincome = fct_reorder(rincome, age))  %>% 
  ggplot(aes(x= age, y = rincome)) +
    geom_point()

rincome_summary %>%
  ggplot(aes(x= age, y = rincome)) +
    geom_point()
  

