# Gathering and Sreading
# Initial: 22 Oct 2018
# Revision: 19 Feb 2019
# Ray Nelson

# Libraries
library(tidyverse)

# Load workspace
fitness <- read_csv(url("http://govfinance.byu.edu/govfinance/classes/dataScience/lectures/illustrations/Fitness.csv"))
movies <- read_csv(url("http://govfinance.byu.edu/govfinance/classes/dataScience/lectures/illustrations/Movies.csv"))
load(url("http://govfinance.byu.edu/govfinance/classes/dataScience/lectures/illustrations/movies_fitness.RData"))

# Movies
Movies %>% glimpse()
Movies %>% summary()

## Distribution of Type
Movies %>% count(Type)
Movies %>%
  ggplot(aes(x = Type, fill = Type)) +
    geom_bar(show.legend = FALSE)

## Distribution of Rating
Movies %>% count(Rating)
Movies %>%
  ggplot(aes(x = Rating, fill = Rating)) +
  geom_bar(show.legend = FALSE)

# Time Series Trends of revenue
Movies %>%
  group_by(Year) %>% 
  summarize(Domestic_median = median(Domestic),
            Worldwide_median = median(Worldwide)) %>% 
  gather(key = Source, value = Revenue, -Year) %>% 
  ggplot(aes(x = Year, y = Revenue, color = Source)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Compare Domestic and Worldwide Revenues
movies <- Movies %>%
  select(Rating, Year, Domestic, Worldwide) %>% 
  gather(key = source, value = revenue, -c("Rating", "Year"))

colnames(movies) <- c("rating", "year", "source", "revenue")

movies %>% ggplot(aes(x = source, y = revenue, fill = source)) +
  geom_boxplot(show.legend = FALSE) + 
  facet_grid(rows = vars(rating)) +
  coord_flip()

## Only Domestic

movies %>% 
  filter(source == "Domestic") %>% 
  ggplot(aes(x = rating, y = revenue, fill = rating)) +
    geom_boxplot(show.legend = FALSE) + 
    coord_flip()

# Revenue by domestic and Rating

movies %>% 
  ggplot(aes(x = rating, y = revenue, fill = rating)) +
  geom_boxplot(show.legend = FALSE) + 
  coord_flip() + 
  facet_grid(rows = vars(source))

# Fitness
## Explore Fitness
Fitness %>%
  glimpse()
Fitness %>%
  summary()
Fitness %>%
  count(Sex)

## Comparison of pulse by Sex
Fitness %>% 
  select(Sex, RunPulse, RstPulse, MaxPulse) %>% 
  gather(key = Pulse_Type, value = Pulse, -Sex) %>% 
  ggplot(aes(x = Sex, y = Pulse, fill = Sex)) +
    geom_boxplot(show.legend = FALSE) +
    facet_grid(rows = vars(Pulse_Type)) +
    coord_flip()

# Violin plot of MaxPuse and RunPulse by Sex
Fitness %>% 
  select(Sex, RunPulse, MaxPulse) %>% 
  gather(key = Pulse_Type, value = Pulse, -Sex) %>% 
  ggplot(aes(x = Sex, y = Pulse, fill = Sex)) +
    geom_violin(fill = "grey") +
    geom_boxplot(width = 0.25, show.legend = FALSE) +
    facet_grid(rows = vars(Pulse_Type)) +
    coord_flip()

# Violin Plot of Pulse_type by Gender
Fitness %>% 
  select(Sex, RunPulse, MaxPulse) %>% 
  gather(key = Pulse_Type, value = Pulse, -Sex) %>% 
  ggplot(aes(x = Pulse_Type, y = Pulse, fill = Sex)) +
    geom_violin(fill = "grey") +
    geom_boxplot(width = 0.25, show.legend = FALSE) +
    facet_grid(rows = vars(Sex)) +
    coord_flip()

# Relationship of RunPulse and MaxPulse by Gender

Fitness %>% 
  ggplot(aes(x = RunPulse, y = MaxPulse, color = Sex)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

# Extracting components
regression <- lm(MaxPulse ~ RunPulse, data = Fitness )
regression %>% str()
regression %>% coef()
regression %>% summary()

# Plot of Predicted versus actual
plot_data <- tibble(regression$fitted.values, Fitness$MaxPulse, Fitness$Sex)
colnames(plot_data) <- c("Fitted", "Actual", "Sex")

plot_data %>% 
  ggplot(aes(x = Fitted, y = Actual)) +
    geom_point(aes(color = Sex)) +
    geom_smooth(method = "lm", color = "grey")
