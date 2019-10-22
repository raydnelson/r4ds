# Spreading and gathering
# Initial: October 16, 2018
# Revision: October 17, 2018
# Ray Nelson

# Load libraries
library(tidyverse)
library(fImport)
library(zoo)
library(Quandl)

# Different Data formats
table1
table2
table3
table4a # cases
table4b # population

# Examples of the value of tidy data
## rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)

## cases per year
table1 %>% 
  count(year, wt = cases)

## Visualization of changes over time
table1 %>% 
  ggplot(aes(x = year, y = cases)) +
    geom_line(aes(group = country), colour = "grey50", size = 1) +
    geom_point(aes(colour = country), size = 2) +
    labs(
      title = "Tuberculosis Cases",
      subtitle = "1999 and 2000",
      x = "",
      y = "Number of Cases"
    )

# Gathering
table4a
table4a %>%
  gather(`1999`, `2000`, key = year, value = cases)

table4b
table4b %>% 
  gather(`1999`, `2000`, key = year, value = population)

# All the data together into a tidy tibble
tidy4a <- table4a %>%
  gather(`1999`, `2000`, key = year, value = cases)

tidy4b <- table4b %>%
  gather(`1999`, `2000`, key = year, value = population)

left_join(tidy4a, tidy4b)

# Spreading
table2

table2 %>%
  spread(key = type, value = count)

# 12.3.3 Exercises

## 1. Why are gather and spread not perfectly symmetrical
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks

stocks %>% 
  spread(key = year, value = return)

stocks %>% 
  spread(key = year, value = return) %>% 
  gather(key = year, value = "return")

stocks %>% 
  spread(key = year, value = return) %>% 
  gather(`2015`, `2016`, key = year, value = "return")

## 2. Why does this code fail?

table4a %>% 
  gather(1999, 2000, key = "year", value = "cases")

table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

## 3. Why does spreading this tibble value
people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people %>%
  spread(key = key, value = value)

people <- tribble(
  ~name,             ~key,    ~value,  ~id,
  #-----------------|--------|--------|----
  "Phillip Woods",   "age",       45,   1,
  "Phillip Woods",   "height",   186,   1,
  "Phillip Woods",   "age",       50,   2,
  "Jessica Cordero", "age",       37,   3,
  "Jessica Cordero", "height",   156,   3,
)

people %>%
  spread(key = key, value = value) %>% 
  arrange(id)

## Spread Table 2
table2 %>%
  spread(key = type, value = count)

# Calculate 
table2
cases <- table2[table2$type == "cases", ] %>% select(-type)
colnames(cases) <- c("country", "year", "cases")
population <- table2[table2$type == "population", ] %>% select(-type)
colnames(population) <- c("country", "year", "population")

left_join(cases, population)

# Tidy the pregnacy table
preg <- tribble(
  ~pregnant, ~male, ~female,
  #---------\------\---------
  "yes",     NA,    10,
  "no",      20,    12
)

preg %>% gather(male, female, key = gender, value = count, na.rm = TRUE)

# Gather Example

stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

stocks %>% gather(key = stock, value = price, -time)

# Another Gather example using the iris data

mini_iris <- iris %>%
  mutate(row = row_number()) %>% 
  filter(row %in% c(1, 51, 101)) %>% 
  select(-row)

mini_iris %>% 
  gather(key = flower_att, value = measurement, -Species) %>% 
  arrange(Species)

# Example of how time series gathering and spreading
# Get time series functions
"http://govfinance.byu.edu/govfinance/quantTools/R/manipulations.R" %>%
  url() %>%
  source()

# Time series of income and sales taxes in Utah using from FRED using fImport
revenue <- fredSeries(c("UTSLGRTAX", "UTINCTAX"), from = "1992-01-01") %>%
  as.zoo() %>% 
  pca()
colnames(revenue) <- c("sales", "income")
revenue
revenue %>% str()

# Extract the date from the zoo object
dates <- revenue %>%
  time() %>%
  as.Date()

# Create a tibble
revenue_tbl <- tibble(dates, as.vector(revenue$sales), as.vector(revenue$income))
colnames(revenue_tbl) <- c("dates", "sales", "income")
revenue_tbl

# Create a data frame
revenue_df <- data.frame(dates, revenue)
colnames(revenue_df) <- c("dates", "sales", "income")
revenue_df

# Time series of income and sales taxes in Utah using Quandl
revenue <- Quandl(c("FRED/UTSLGRTAX", "FRED/UTINCTAX"),
                  transform = "rdiff") %>% as.tibble()
colnames(revenue) <- c("dates", "sales", "income")
revenue_tbl <- revenue %>% mutate(sales = sales * 100, income = income * 100)
revenue_tbl %>% str()
revenue_tbl

# Graph of sales by itself
revenue_tbl %>% 
  ggplot(aes(x = dates, y = sales)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = "Utah Sales Tax Growth Rate",
         subtitle = "1992 - 2016",
         x = "",
         y = "Percentage Change")

revenue_tbl %>% 
  ggplot(aes(x = dates, y = income)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red")  +
  labs(title = "Utah Income Tax Growth Rate",
       subtitle = "1992 - 2016",
       x = "",
       y = "Percentage Change")

# Gather the data together
plot_data <- revenue_tbl %>% 
  gather(sales, income, key = "tax", value = "revenue")

# Time series plot of both in the same graph
plot_data %>% 
  ggplot(aes(x = dates, y = revenue, color = tax)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = "Utah Sales and Income Tax Growth Rates",
         subtitle = "1992 - 2016",
         x = "",
         y = "Percentage Change")

# Time sries plot of each in its own panel
plot_data %>% 
  ggplot(aes(x = dates, y = revenue)) +
    geom_line(aes(color = tax), show.legend = FALSE) +
    facet_grid(rows = vars(tax)) +
    geom_hline(yintercept = 0, color = "red", alpha = 0.25) +
    labs(title = "Utah Sales and Income Tax Growth Rates",
         subtitle = "1992 - 2016",
         x = "",
         y = "Percentage Change")

# Violin Plot
plot_data %>%
   ggplot(aes(x = tax, y = revenue)) +
    geom_violin(fill = "lightgreen") +
    geom_boxplot(width = 0.25, fill = "lightblue") +
    labs(title = "Comparison of Sales and Income Tax Growth Rates",
         subtitle = "State of Utah, 1992 - 2016",
      x = "Tax",
      y = "Percentage Change") +
    coord_flip()
