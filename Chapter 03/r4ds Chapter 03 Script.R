# R for Data Science Chapter 03
# Initial: May 17, 2018
# Revision: May 18, 2018
# Ray Nelson

library(tidyverse)

# 3.2.1 The mpg data frame
mpg

# 3.2.2 Creating a ggplot
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# 3.2.4 Exercises

# 1
ggplot(data = mpg)

# 2
? mpg
mpg %>% nrow()
mpg %>% ncol()

# 3
? mpg

# 4
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))

# 5
ggplot(data = mpg) +
  geom_point(mapping = aes(x = drv, y = class))

# 3.3 Aesthetic mappings
# Color
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# Size
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Transparency
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Shape
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# Blue
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# 3.3.1 Exercises