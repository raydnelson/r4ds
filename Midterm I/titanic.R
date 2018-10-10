# Midterm I Titanic Dataset
# Initial: October 4, 2018
# Revision: October 9, 2018
# Ray Nelson

# Load libraries
library(tidyverse)

# summary of all variables
titanic %>% summary()

# Passenger class and sex survival
titanic %>% ggplot(aes(x = pclass, fill = survived)) +
  geom_bar() +
  facet_grid(rows = vars(sex))
titanic %>% ggplot(aes(x = pclass, fill = survived)) +
  geom_bar(position = "fill") +
  facet_grid(rows = vars(sex))

# Age
titanic %>% ggplot(aes(x = sex, y = age)) +
  geom_violin(aes(fill = sex), show.legend = FALSE) +
  geom_boxplot(width = 0.25, fill = "grey") +
  labs(title = "Titanic Passengers",
       subtitle = "By Gender",
       x = "Sex",
       y = "Years") +
  coord_flip()

# Fare
titanic %>%
  group_by(pclass) %>% 
  summarise(median = median(fare, na.rm = TRUE),
            mean = mean(fare, na.rm = TRUE))

titanic %>% ggplot(aes(x = pclass, y = fare)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Fare Paid by Titanic Passengers",
       subtitle = "All passengers",
       x = "Passenger Class",
       y = "Dollars") +
  coord_flip()

titanic %>% ggplot(aes(x = survived, y = fare, fill = survived)) +
    geom_boxplot(aes(fill = survived), show.legend = FALSE) +
    labs(title = "Possible effect of fare price on survival",
      x = "Survival",
      y = "Dollars") +
    coord_flip()

# Scatterplot of age on fare
titanic %>%
  filter(fare < 200) %>% 
  ggplot(aes(x = age, y = fare, color = survived)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(
      title = "Relationship between age and fare",
      subtitle = "Passengers who paid less than $200",
      x = "Years",
      y = "Dollars"
    )

# Age by passenger class, gender, and survival
titanic %>%
  ggplot(aes(x = pclass, y = age, fill = sex)) +
    geom_boxplot() +
    facet_grid(rows = vars(survived, sex)) +
    labs(title = "Comparison of fares by passenger class, gender, and survival",
      subtitle = "All passengers",
      x = "Passenger Class",
      y = "Dollars") +
  coord_flip()





