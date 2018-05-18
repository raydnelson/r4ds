# R for Data Science Chapter 03
# Initial: May 16, 2018
# Revision: May 17, 2018
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
mpg %>% dim()
mpg %>% glimpse()

# 3
? mpg

# 4
ggplot(data = mpg) +
  geom_point(mapping = aes(x = hwy, y = drv))

# 5
ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))

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

# 1
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# 2
? mpg

# 3
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = cty))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = cty))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = cty))

# 4
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy,
                           color = drv,
                           shape = drv,
                           size = drv))


# 5

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy),  stroke = 3, size = 2,
             color = "white", fill = "red", shape = 22)

# 6
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5)) +
  labs(color = "Displacement Categories")

# 3.5 Facets

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

# 3.5.1 Exercises

# 1
ggplot(data = mtcars) +
  geom_point(mapping = aes(x = disp, y = mpg)) +
  facet_grid(~ wt)

# 2
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))

# 3
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# 4

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# 5
? facet_wrap

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) + 
  facet_wrap(~ class, ncol = 2)

# 3.5 Geometric Objects
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy), method = "loess")

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv), method = "loess")

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(mapping = aes(linetype = drv), method = "loess")

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy), method = "loess")

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv), method = "loess")

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE,
    method = "loess"
  )

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy), method = "loess")

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(method = "loess")

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(method = "loess")

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE,
              method = "loess")

# 3.6.1 Exercises

# 1

ggplot(data = mpg) +
  geom_line(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = drv, y = hwy))

ggplot(data = mpg) +
  geom_histogram(mapping = aes(x = hwy))

ggplot(data = mpg) +
  geom_area(mapping = aes(x = displ, y = hwy))

# 2

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# 3

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# 4

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = TRUE)

# 5

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "In ggplot function call")

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy)) +
  labs(title = "In geom calls")

# 6

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(aes(goup = drv), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth( se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth( se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(mapping = aes(linetype = drv), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(fill = drv), size = 3, stroke = 3, shape = 21, color = "white")

# 3.7 Statistical transformations

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut)) +
  labs(title = "Plot Using a Geometric Object")

ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut)) +
  labs(title = "Plot Using a Statistical Transformation")

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# 3.7.1 Exercises

# 1

? stat_summary()

# 2

# The bar chart counts frequencies and
# The column expects that the frequencies have already been calculated.

# 3

# look at the documentatin in tidyverse

# 4
# stat_smooth
# predicted, ymin, ymax, and se

# 5

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop.., group = color))

# 3.8 Position adjustments

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, color = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

# 3.8.1 Exercises

# 1. Problem of overplotting so we use jitter
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(position = "jitter")

# 2. Jitter parameters

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(position = "jitter") +
  geom_smooth()

# 3. Paramters for geom_jitter()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point() +
  geom_jitter() +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point() +
  geom_jitter(width = 0.5, height = 0.5) + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point() +
  geom_count()

# 4. Jitter and count geom's

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_count()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(position = "jitter")

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point(alpha = 1 / 5)

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  stat_density_2d(aes(fill = ..x..))

# 5. Default for geom_boxplot

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = drv, y = hwy, fill = drv), notch = TRUE)

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = drv, y = hwy, fill = drv), notch = TRUE) +
  coord_flip()

# 3.9 Coordinate Systems

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "lightgreen", colour = "black") +
  coord_quickmap() +
  labs(title = "Map of New Zealand",
       x = "",
       y = "")

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

# 3.9.1 Exercises

# 1
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill") +
  coord_polar(theta = "y")

ggplot(data = mpg, mapping = aes(x = factor(1), fill = class)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y")

# 2
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "identity") +
  coord_polar() +
  labs(title = "Pie chart from stacked bar chart",
       subtitle = "Diamonds data set",
       x = "Cut of diamond",
       y = "Count",
       fill = "Clarity")

# 3

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "lightgreen", colour = "black") +
  coord_quickmap()

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "lightgreen", colour = "black") +
  coord_map()

# 4

m <- ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point(position = "jitter") + 
  geom_smooth(method = "lm") +
  geom_abline() +
  coord_fixed()

