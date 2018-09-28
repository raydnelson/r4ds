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

batters <- 
  
## Most at bats, hits, and home runs
# Calculate totals over season (at_bat, hits, home_runs)

totals <- 

# At bats
totals %>% 

# Hits
totals %>% 

# Home Runs
totals %>% 

## Best batters
# Compute batting_average and home_run_average
  
productivity <- totals %>%
  

# Highest Batting average with playerID, at_bat, hits, batting_average
productivity %>% 

# Most home runs per at bat with playerID, at_bat, home_runs, home_run_average
productivity %>% 


# Productivity comparison of home run leaders
productivity %>%


## Graphs of top 100 home run hitters
# New variable to rank players by number of home runs hit
# Choose the top 100 home run hitters
# Draw Graph
  
productivity %>%

## Relationship between home run average and batting average for top 100 hitters
# Create a variable that gives the player ranking in batting average
# Choose the top 100 batters
# Use ggplot to draw a smoothed scatterplot

productivity %>% 


# Comparison of at bats, hits, and home runs by season
## Create a new grouping by season
seasons <- batting %>% group_by(yearID)

## Calculate the totals
season_totals <- seasons %>%

## Graph of at Bats by season
season_totals %>%

# Convert tibble to long form
season_totals_long <- season_totals %>% 
  gather(measure, totals, -yearID)

# Use ggplot to draw a graph of at bats, hits, and home runs

season_totals_long %>%
