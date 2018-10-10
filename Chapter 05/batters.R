# Group 
# Initial: September 25, 2018
# Revision: September 26, 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(Lahman)

# Initial tibble
batting <- Lahman::Batting %>% as_tibble()
batters <- batting %>% group_by(playerID)

# Exclude players to didn't bat
batters <- batters %>% filter(AB > 0)

## Most at bats, hits, and home runs
# Calculate totals over season (at_bat, hits, home_runs)

totals <- batters %>% 
  summarize(at_bat = sum(AB, na.rm = TRUE),
            hits = sum(H, na.rm = TRUE),
            home_runs = sum(HR, na.rm = TRUE)
            )

# At bats
totals %>% arrange(at_bat %>% desc())

# Hits
totals %>% arrange(hits %>% desc())

# Home Runs
totals %>% arrange(home_runs %>% desc())

## Best batters
# Compute batting_average and home_run_average

productivity <- totals %>%
  mutate(batting_average = hits / at_bat, home_run_average = home_runs / at_bat)
  


# Highest Batting average with playerID, at_bat, hits, batting_average
productivity %>% filter(at_bat > 1000) %>% 
  arrange(batting_average %>% desc()) %>% 
  select(playerID, at_bat, hits, batting_average)

# Most home runs per at bat with playerID, at_bat, home_runs, home_run_average
productivity %>% 
  arrange(home_run_average %>% desc()) %>% 
  select(playerID, at_bat, home_runs, home_run_average)

# Productivity comparison of home run leaders
productivity %>%
  arrange(home_runs %>% desc())

## Graphs of top 100 home run hitters
# New variable to rank players by number of home runs hit
# Choose the top 100 home run hitters
# Draw Graph
productivity %>%
  mutate(home_run_rank = home_runs %>% desc() %>% min_rank) %>% 
  filter(home_run_rank <= 100) %>% 
  ggplot(mapping = aes(x = at_bat, y = home_runs)) +
    geom_point() +
    geom_smooth() +
    labs(title = "Home Run Leaders",
         subtitle = "Top 100",
         x = "Number of Times at Bat",
         y = "Total Number of Home Runs")

## Relationship between home run average and batting average for top 100 hitters
# Create a variable that gives the player ranking in batting average
# Choose the top 100 batters
# Use ggplot to draw a smoothed scatterplot
productivity %>% 
  mutate(batting_rank = batting_average %>% desc() %>% min_rank()) %>% 
  filter(batting_rank <= 100) %>% 
  ggplot(mapping = aes(x = batting_average, y = home_run_average)) +
    geom_point() +
    geom_smooth() +
    labs(title = "Relationship Between Home Run Percentage and Batting Average",
         subtitle = "Top 100 Batting Average",
         x = "Batting Average",
         y = "Home Run Percentage")

# Comparison of at bats, hits, and home runs by season
## Create a new grouping by season
seasons <- batting %>% group_by(yearID)

## Calculate the totals
season_totals <- seasons %>% 
  summarise(
    at_bats = sum(AB, na.rm = TRUE),
    hits = sum(H, na.rm = TRUE),
    home_runs = sum(HR, na.rm = TRUE)
  )

# Graph of at Bats
season_totals %>%
  ggplot(mapping = aes(x = yearID, y = at_bats )) +
    geom_point() +
    geom_smooth(span = 0.2)

# Convert tibble to long form
season_totals_long <- season_totals %>% 
  gather(measure, totals, -yearID)

# Use ggplot to draw a graph of at bats, hits, and home runs

season_totals_long %>%
  ggplot(mapping = aes(x = yearID, y = totals, color = measure)) +
    geom_point(show.legend = FALSE) +
    geom_smooth(show.legend = FALSE, span = 0.2) +
    facet_wrap(vars(measure), ncol = 1, scales = "free_y") +
    labs(title = "Major League Baseball Trends",
         x = "",
         y = "Totals")
