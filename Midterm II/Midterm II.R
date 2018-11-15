# Midterm II
# Initial: 2 November 2018
# Revision: 6 November 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(RSQLite)
library(lubridate)

# Read in data from database

## connect to database
con <- dbConnect(drv = SQLite(), dbname = "Midterm II/soccer.sqlite")

# Read Each Table
player <- dbReadTable(con, "Player") %>% as.tibble()
player_attributes <- dbReadTable(con, "Player_Attributes") %>% as.tibble()

# Disconnect from database
dbDisconnect(con)
rm(con)

# Select and rename columns
player <- player %>%
  select(player_api_id, player_name, birthday) %>% 
  mutate(birthday = as_date(birthday))
colnames(player) <- c("player_id", "player_name", "birthday")

player_attributes <- player_attributes %>% 
  select(player_api_id, date, overall_rating) %>% 
  mutate(date = as_date(date))
colnames(player_attributes) <- c("player_id", "match_date", "rating")

# Write data frames to csv

# write.csv(player, "player.csv", row.names = FALSE)
# write.csv(player_attributes, "player_attributes.csv", row.names = FALSE)

# player_test <- read_csv("midterm II/player.csv")
# player_attributes_test <- read_csv("midterm II/player_attributes.csv")

# Convert match_date and birthdays into dates using as_date

# Create a factor for each season from the match_date variable
seasons <- 2007:2016
player_attributes <- player_attributes %>% 
  mutate(season = year(match_date) %>% factor(levels = seasons))

# Number of players participating in each year
player_attributes %>% 
  group_by(player_id, season) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = season, fill = season)) +
    geom_bar(show.legend = FALSE) +
    labs(
      title = "Number of Players Participating in Each Year",
      subtitle = "2007 - 2016",
      x = "Season",
      y = "Number of Players"
    )

# Number of seasons played
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

# Calculate the age of each player for the season played
## Function to calculate the age as of 1 July 2016
age <- function(birthday, match_date){
  (birthday %--% match_date) / years(1)
}

player_attributes_complete <- player_attributes %>% 
  left_join(player, key = "player_id") %>% 
  mutate(age = age(birthday, match_date))

# Violin plot of the ages of those that participated in 2016
player_attributes_2016 <- player_attributes_complete %>% 
  filter(year(match_date) == 2016)

player_attributes_2016 %>% 
  ggplot(aes(x = factor(0), y = age)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = 0.25, fill = "lightgreen") +
  coord_flip() +
  labs(
    title = "Age Distribution of Soccer Players",
    subtitle = "2016 Season",
    x = "",
    y = "Years"
  )

# Relationship of Age and Average Rating for 2016 players
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

# Time series of average age and average rating
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



