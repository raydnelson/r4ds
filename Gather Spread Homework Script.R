# Gathering and Spreading Homework
# Initial: 19 Feb 2019
# Revision: 19 Feb 2019
# Ray Nelson

# Libraries
library(tidyverse)

# MPG Data Set
data(mpg)

mpg %>% 
  select(cty, hwy) %>% 
  gather(cty, hwy, key = type_of_driving, value = mileage) %>% 
  group_by(type_of_driving) %>% 
  summarise(
    Means= mean(mileage),
    Medians = median(mileage),
    IRQs = IQR(mileage),
    Standard_Deviations = sd(mileage)
  )

mpg %>%
  summarise(
    mean_city = mean(cty),
    mean_highway = mean(hwy),
    median_city = median(cty),
    median_highway = median(hwy),
    IQR_city = IQR(cty),
    IQR_highway = IQR(hwy),
    sd_city = sd(cty),
    sd_highway = sd(hwy)
  )

mpg %>%
  select(cty, hwy) %>%
  gather(cty, hwy, key = type_of_driving, value = mileage) %>%
  ggplot(aes(x = type_of_driving, y = mileage)) +
  geom_violin(fill = "lightgreen") +
  geom_boxplot(width = 0.25, fill = "lightblue") +
  labs(title = "Highway mileage is larger than city mileage",
       x = "Type of Driving",
       y = "Miles Per Gallon") +
  coord_flip()
