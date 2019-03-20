# Midterm II Scripts
# Initial: 2 November 2018
# Revision: 6 November 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(lubridate)

# 5. Number of seasons played
number_of_seasons <- player_attributes %>% 
  group_by(player_id, season) %>% 
  count() %>% 
  group_by(player_id) %>% 
  count()
colnames(number_of_seasons) <- c("player_id", "number_of_seasons")
number_of_seasons <- number_of_seasons %>% 
  mutate(number_of_seasons = factor(number_of_seasons, levels = 1:10))

number_of_seasons %>% 
  ggplot(aes(x = number_of_seasons, fill = number_of_seasons)) +
    geom_bar(show.legend = FALSE) +
    labs(
      title = "Longevity of Soccer Players",
      subtitle = "European Soccer Leagues",
      x = "Number of Seasons",
      y = "Number of Players"
    )

### 7. Player attributes
  left_join(player, key = "player_id") %>% 
  mutate(age = age(birthday, match_date))

### 9 Relationship of Age and Average Rating for 2016 players
  
player_attributes_2016 %>% 
  group_by(player_id) %>% 
  summarize(
    average_rating = mean(rating)
  ) %>% 
  left_join(player, key = "player_id") %>% 
  mutate(age = age(birthday, ymd(20160701))) %>% 
  ggplot(aes(x = age, y = average_rating)) +
  geom_point(position = "jitter", alpha = 1/5, shape = 19, color = "red") +
  geom_smooth() +
  labs(
    title = "Relationship of Player Rating and Age",
    subtitle = "2016 Soccer Year or European Soccer League",
    x = "Years",
    y = "Average Rating"
  )

### 10. Time series of average age and average rating
player_attributes_complete %>% 
  group_by(season) %>% 
  summarize(
    Age = mean(age),
    Rating = mean(rating, na.rm = TRUE)
  ) %>% 
  gather(Age, Rating, key = "measure", value = "amount")  %>% 
  ggplot(aes(y = amount, color = measure)) +
    geom_point(aes(x = season), show.legend = FALSE) +
    geom_path(aes(x = as.numeric(season)), show.legend = FALSE) +
    facet_grid(rows = vars(measure), scales = "free_y") +
    labs(
      title = "Trends in Average Age and Average Rating",
      subtitle = "European Soccer League (Kaggle Dataset",
      x = "",
      y = ""
    )
