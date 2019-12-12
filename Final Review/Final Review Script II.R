# Companies Data Set for Review
# Initial: 11 December 2018
# Revision: 09 December 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(readxl)

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

# Function to calculate the location and scale of a variable
summary_stats <- function(variable){
  location <- median(variable)
  scale <- IQR(variable)
  c(location, scale)
}

summary_stats(companies$sales)
summary_stats(companies$profits)

# Summary statistics by type for sales and profits
companies %>% 
  select(type, sales, profits) %>% 
  group_by(type) %>% 
  summarize(location_sales = median(sales),
            scale_sales = IQR(sales),
            location_profits = median(profits),
            scale_profits = IQR(profits))

# Summary statistics using gather and group_by
companies %>% 
  select(type, sales, profits) %>% 
  gather(sales, profits, key = "account", value = "dollars") %>%
  group_by(type, account) %>% 
  summarise(location = median(dollars),
            scale = IQR(dollars)
         )

# Summary statistics using a loop
sales_profits <- companies %>% 
  select(type, sales, profits)

output <- list(sales = NULL, profits = NULL)

for(i in c("sales", "profits")){
  analysis_tibble <- sales_profits %>% 
    select(type, i)
  colnames(analysis_tibble) <- c("type", "variable")
  output[[i]] <- analysis_tibble %>%
    group_by(type) %>%
    summarise(
      location = median(variable),
      scale = IQR(variable)
    )
}

account <- rep(c("sales", "profits"), each = 2)
rbind(output$sales, output$profits) %>% 
  add_column(account) %>% 
  select(type, account, location, scale) %>% 
  arrange(type, account)

# Summary statistics using map in a function
summaries <- function(type) {
  companies %>% 
    filter(type == type) %>% 
    select(sales, profits) %>% 
    map(summary_stats)
}

output <- list(Computer = NULL, Pharmaceuticals = NULL)
for(i in c("Computer", "Pharmaceuticals")){
  output[[i]] <- summaries(i)
}

output

# Loop over type

# Type of company and the linear model with common slope
companies_model <- lm(profits ~ sales + type, data = companies)
companies_model %>% summary()

# geom_smooth estimates separate slopes and separate intercepts for each type
companies %>% 
  ggplot(aes(x = sales, y = profits, color = type)) + 
    geom_point() +
    geom_smooth(method = "lm")

# Without the large sales company
companies %>% 
  filter(sales < max(sales)) %>% 
  ggplot(aes(x = sales, y = profits, color = type)) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

# Different slopes and different intercepts
lm(profits ~ type + sales + sales * type,
                      data = companies %>% filter(sales < max(sales))) %>% 
  summary()

