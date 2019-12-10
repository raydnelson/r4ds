# Final Exam
# Initial: 16 Apr 2019
# Revision: 16 Apr 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Code Interpretation

# Pareto Charts
mpg %>% 
  group_by(class) %>% 
  mutate(nobs = n()) %>% 
  ggplot(aes(fct_reorder(class, nobs) %>% fct_rev(), nobs)) +
  geom_col()

mpg %>% 
  ggplot(aes(class %>% fct_infreq())) +
    geom_bar()

# Linear model with mtcars
mtcars$vs <- factor(mtcars$vs, levels = c(0,1), labels = c("automatic", "manual"))
mtcars_lm <- lm(mpg ~ wt + vs + vs * wt, data = mtcars)
new_data <- tibble(wt = c(3, 4), vs = c("automatic", "manual"))
mtcars_lm %>% predict(newdata = new_data)

# Data manipulation
flights %>% 
  filter(origin != "EWR") %>% 
  filter(dep_delay > 0) %>% 
  group_by(origin) %>% 
  summarise(
    median_delay = median(dep_delay, na.rm = TRUE),
    IQR_delay = IQR(dep_delay, na.rm = TRUE)
  ) %>% 
  left_join(airports, c("origin" = "faa")) %>% 
  select(name, median_delay, IQR_delay) %>% 
  arrange(median_delay)

# Presidential
presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, color = party)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = end, yend = id), size = 3) +
  scale_x_date(breaks = presidential$start, date_labels = "'%y") +
  scale_y_continuous(breaks = seq(34, 44, 1), labels = presidential$name) +
  scale_colour_manual(values = c("Republican" = "red", "Democratic" = "blue")) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom"
  )

# function and iteration
Mean <- function(x) {
  if(is.numeric(x)){
    mean(x)
  } else {
    NA
  }
}

means <- vector("numeric", length(mpg))
for (i in seq_along(mpg)) {
  means[[i]] <- Mean(mpg[[i]])
}

tibble(means) %>% 
  filter(!is.na(means))
