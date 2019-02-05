# Covariation
# Initial: October 2, 2018
# Revision: October 3, 2018
# Ray Nelson

# load libraries
library(tidyverse)

# Review of the variation
## Bar Chart for cut
diamonds %>% ggplot(mapping = aes(x = cut, fill = cut)) +

## Histogram for price
diamonds %>% ggplot(mapping = aes(x = price)) +

## Density trace for price
diamonds %>% ggplot(mapping = aes(x = price)) +


## Histogram and density trace for price
diamonds %>% ggplot(mapping = aes(x = price)) +
  geom_histogram(aes(y = ..density..), fill = "lightgreen") +


## Boxplot of price
diamonds %>% ggplot(mapping = aes(x = factor(0), y = price)) +


## Violin plot of price
diamonds %>% ggplot(mapping = aes(x = factor(0), y = price)) +

## Combination of boxplot and violin plot of price
diamonds %>% ggplot(mapping = aes(x = factor(0), y = price)) +
  geom_violin(fill = "lightgreen") +
  geom_boxplot(fill = "lightblue", width = 0.15) +
  coord_flip()

# Covariation of a continuous variable and numberical
## Histogram
diamonds %>% ggplot(mapping = aes(x = price, fill = cut)) +
  geom_histogram(binwidth = 500, alpha = 0.25)

## Freqpoly
diamonds %>% ggplot(mapping = aes(x = price, color = cut)) +
  geom_freqpoly(bindwidth = 500)

diamonds %>% ggplot(mapping = aes(x = price, y = ..density.., color = cut)) +
  geom_freqpoly(bandwidth = 500)

# Density
diamonds %>% ggplot(mapping = aes(x = price, fill = cut)) +
  geom_density(alpha = 0.25)

## Boxplot
diamonds %>% ggplot(mapping = aes(x = cut, y = price, fill = cut)) +
  geom_boxplot() +
  coord_flip()

## Violin plot
diamonds %>% ggplot(mapping = aes(x = cut, y = price)) +
  geom_violin(mapping = aes(fill = cut)) +
  geom_boxplot(fill = "lightblue", width = 0.05) +
  coord_flip()

# Car Highway Mileage
## Distribution of mileage
mpg %>% ggplot(aes(x = hwy)) +


## Distribution of class of car
mpg %>% ggplot(aes(x = class, fill = class)) +


## Violin plot
mpg %>% ggplot(mapping = aes(x = reorder(class, hwy, FUN = IQR), y = hwy)) +
  geom_violin(fill = "lightgreen") +
  geom_boxplot(width = 0.1, fill = "lightblue") +
  labs(title = "Violin Plot of Mileage", x = "Class of Vehicle", y = "Miles Per Gallon") +
  coord_flip()

## Twp categorical variables
diamonds %>% ggplot(aes(x = cut, y = color)) +
  geom_point(position = "jitter", alpha = 0.8, shape = 16, size = 0.5)

diamonds %>% ggplot(aes(x = cut, y = color)) +
  geom_count()

diamonds %>% 
  count(color, cut) %>% 
  ggplot(mapping = aes(x = cut, y = color)) +
    geom_tile(mapping = aes(fill = n))

# Two continuous variables

# Scatterplot
diamonds %>% ggplot(mapping = aes(x = carat, y = price)) +


# Two Dimensional Density
## choose the smaller diamonds
set.seed(4393)
dsmall <- diamonds %>% sample_n(1000)
dsmall %>% ggplot(mapping = aes(x = carat, y = price)) +
  geom_point()

dsmall %>% ggplot(mapping = aes(x = carat, y = price)) +
  geom_bin2d()

dsmall %>% ggplot(mapping = aes(x = carat, y = price)) +
  geom_hex()

dsmall %>% ggplot(mapping = aes(x = carat, y = price)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon")

dsmall %>% ggplot(aes(x, y)) +
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE)

## Small diamonds with boxplots
smaller <- diamonds %>% filter(carat < 3)
smaller %>% ggplot(mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1))) +
  coord_flip()

## Small number of diamonds with same number of points per bin
smaller %>% ggplot(mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

# Patterns
## Density for eruptions
faithful %>% ggplot(mapping = aes(x = eruptions)) +
  geom_density(fill = "lightblue")

# Density for waiting
faithful %>% ggplot(mapping = aes(x = waiting)) +
  geom_density(fill = "lightblue")

# Scatterplot for waiting and eruptions
faithful %>% ggplot(mapping = aes(x = waiting, y = eruptions)) +
  geom_point()

# Is there a linear pattern in the scatterplot
faithful %>% ggplot(mapping = aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm")

# Is there a nonlinear pattern in the scatterplot
faithful %>% ggplot(mapping = aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "loess")

# Two dimensional density function (heat map)
faithful %>% ggplot(mapping = aes(x = waiting, y = eruptions)) +
  stat_density_2d(aes(fill = stat(density)), geom = "raster", contour = FALSE)

# Two dimension density function (contour plot)
faithful %>% ggplot(mapping = aes(x = waiting, y = eruptions)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon")

# Models

library(modelr)
# Fit a linear model to remove the effect on size on diamond prices
mod <- lm(log(price) ~ log(carat), data = diamonds)

# Calculate the residuals and add them to a new data frame
diamonds2 <- diamonds %>%
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

# catterplot of residuals by diamond size
diamonds2 %>% ggplot(mapping = aes(x = carat, y = resid)) +
  geom_point() +
  geom_smooth()

# Violin plot of prices after the effect of weight has been removed
diamonds2 %>% ggplot(mapping = aes(x = cut, y = resid)) +
  geom_violin(fill = "lightgreen") +
  geom_boxplot(width = 0.25, fill = "lightblue") +
  labs(title = "Prices with the effect of diamonds weight removed",
  x = "Cut of Diamond",
  y = "Residuals in Dollars") +
  coord_flip()

# Violin plot of the residuals before the effect of diamond weight has been removed
diamonds %>% ggplot(mapping = aes(x = cut, y = price)) +
  geom_violin(fill = "lightgreen") +
  geom_boxplot(width = 0.08, fill = "lightblue") +
  labs(title = "Prices with the effect of diamond weight",
  x = "Category",
  y = "Variable") +
  coord_flip()

library(ggmosaic)
library(tidyverse)

diamonds %>% 
  group_by(color) %>% 
  count(cut) %>% 
  
  ggplot(data = diamonds) +
  geom_mosaic(aes(x = product(color, cut), fill = color)) +
  facet_grid(rows = vars(clarity))


ggplot(data = diamonds2) +
  geom_mosaic()

data(Titanic)
titanic <- as.data.frame(Titanic)
titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))
ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived))
# good practice: use



