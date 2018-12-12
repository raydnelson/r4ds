# Companies Data Set for Review
# Initial: 11 December 2018
# Revision: 12 December 2018
# Ray Nelson

# Libraries
library(tidyverse)

# Import the data
companies <- read_excel("Final Review/Companies.xlsx")

# Sales by different types of companies
companies %>% 
  ggplot(aes(x = type, y = sales)) +
    geom_violin(fill = "lightgreen") +
    geom_boxplot(width = 0.25, fill = "lightblue") +
    labs(title = "One of the computer companies is very large",
       x = "Type of company",
       y = "Millions of Dollars") +
    coord_flip()

# Calculate location and spread of sales for the different types of companies

companies %>% 
  group_by(type) %>% 
  summarize(location_mean = mean(sales),
            location_median = median(sales),
            scale_sd = sd(sales),
            scale_iqr = IQR(sales))

# Profits by different types of companies
companies %>%
  ggplot(aes(x = type, y = profits)) +
  geom_violin(fill = "lightgreen") +
  geom_boxplot(width = 0.25, fill = "lightblue") +
  labs(title = "Pharmaceduticals are more profitable",
       x = "Type of company",
       y = "Millions of Dollars") +
  coord_flip()

# Calculate the location and scale of sales and profits by type
summary_stats <- function(variable){
  location <- median(variable)
  scale <- IQR(variable)
  c(location, scale)
}

summary_stats(companies$sales)

# Summary statistics by type for sales and profits
companies %>% 
  group_by(type) %>% 
  summarise(
    location = mean(sales),
    scale = sd(sales)
  )

# Summary statistics by type for sales and profits
sales_profits <- companies %>% 
  select(type, sales, profits)

output <- list(sales = NULL, profits = NULL)

for(i in 1:2){
  analysis_tibble <- sales_profits[, c(1, i + 1)]
  colnames(analysis_tibble) <- c("type", "variable")
  output[[i]] <- analysis_tibble %>%
    group_by(type) %>%
    summarise(
      location = mean(variable),
      scale = sd(variable)
    )
}

# Type of company and the linear model

companies_model <- lm(profits ~ sales + type, data = companies)
companies_model %>% summary()

companies %>% 
  ggplot(aes(x = sales, y = profits, color = type)) + 
    geom_point() +
    geom_smooth(method = "lm")

companies %>% 
  filter(sales < max(sales)) %>% 
  ggplot(aes(x = sales, y = profits, color = type)) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)


companies_model <- lm(profits ~ sales * type, data = companies %>% filter(sales < max(sales)))
companies_model %>% summary()
