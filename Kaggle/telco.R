# Telcom Churn Kaggle competition training set
# Initial: October 10, 2018
# Revision: October 10, 2018
# Ray Nelson

# Load libraries and data
library(tidyverse)

load(url("http://govfinance.byu.edu/govfinance/resources/dataSets/titanic.RData"))
load(url("http://govfinance.byu.edu/govfinance/resources/dataSets/telco.RData"))

telco$Churn <- recode(telco$Churn, Yes = "Left", No = "Stayed")

# Summary of variables
telco %>% summary()

# Are monthly chargers and total charges giving us the same information?
telco %>%
  ggplot(aes(x = MonthlyCharges, y = TotalCharges, color = Churn)) +
    geom_point() +
    geom_smooth()

# Covariation between chur and monthly charges
telco %>% 
  ggplot(aes(x = Churn, y = MonthlyCharges)) +
    geom_boxplot()

# Are monthly charges related to tenure

telco %>% 
  ggplot(aes(x = tenure, y = MonthlyCharges)) +
    geom_point() +
    geom_smooth()

# Does churn seem related to tensure
telco %>% ggplot(aes(x = Churn, y = tenure)) +
    geom_violin(fill = "lightgreen") +
    geom_boxplot(width = 0.1, fill = "lightblue") +
    labs(title = "Are tenure and churn related",
      x = "Churn",
      y = "Tenure") +
    coord_flip()

# Churn and gender
telco %>%
  ggplot(aes(x = gender, fill = Churn)) +
    geom_bar()

# Churn and Senior Citizen
telco %>%
  ggplot(aes(x = SeniorCitizen, fill = Churn)) +
  geom_bar(position = "fill")

# tenure by gender, contract, and senior citizen

telco %>% 
  ggplot(aes(x = Contract, y = tenure)) +
    geom_boxplot(aes(fill = gender)) +
    facet_grid(rows = vars(SeniorCitizen, gender)) +
    coord_flip()

# quitters
quitters <- telco %>% filter(Churn == "Yes")

# Grouping
telco %>% group_by(Churn) %>% summarise(median = median(tenure), IQR = IQR(tenure))

# Who are the people that quit?
telco %>% 
  ggplot(aes(x = InternetService, fill = Churn)) +
    geom_bar()


