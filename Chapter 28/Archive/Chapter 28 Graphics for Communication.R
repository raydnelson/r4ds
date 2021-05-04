# Chapter 28 Graphics for Communication
# Initial: 4 December 2018
# Revision: 4 December 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(ggrepel)
library(viridis)
library(gridExtra)
library(RColorBrewer)

# 28.2 Label -------------------------------------------------------------------

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov",
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)", 
    color = "Car type"
    )

df <- tibble(
  x = runif(10),
  y = runif(10)
)

df %>% ggplot(aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )

## Mathematical equations in labs()

df <- tibble(
  x = runif(10),
  y = runif(10)
)

df %>% ggplot(aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )

# 28.3 Annotations -------------------------------------------------------------

## Best MPG in each car class

(best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)
)

## Scatterplot using geom_text

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_text(aes(label = model), data = best_in_class)

# Scatterplot using geom_label

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_label(aes(label = model), data = best_in_class, nudge_y = 2, alpha = 0.5)

# Scaterplot using geom_label_repel

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_label_repel(aes(label = model), data = best_in_class)

# Annotion on the graph rather than in the legend

(class_avg <- mpg %>%
    group_by(class) %>%
    summarise(
      displ = median(displ),
      hwy = median(hwy)
    )
)

mpg %>% ggplot(aes(displ, hwy, colour = class)) +
  ggrepel::geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6,
                            label.size = 0,
                            segment.color = NA
  ) +
  geom_point() +
  theme(legend.position = "none")

# Annotation in the upper right corner
(label <- mpg %>%
  summarise(
    displ = max(displ),
    hwy = max(hwy),
    label = "Increasing engine size is \nrelated to decreasing fuel economy."
  )
)

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")

# Annotation in the extreme upper right corner

(label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
  )
)

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")


# Positioning

"Increasing engine size is related to decreasing fuel economy." %>%
  stringr::str_wrap(width = 40) %>%
  writeLines()

# Wrapping lines

(label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = "Increasing engine size is related to decreasing fuel economy." %>%
    stringr::str_wrap(width = 40)
)
)

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")

# 28.4 Scales ------------------------------------------------------------------

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(colour = class))

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_discrete()

# 28.4.1 Axis ticks and legend keys

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

## No labels on axes

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)

## 

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, color = party)) +
    geom_point() +
    geom_segment(aes(xend = end, yend = id)) +
    scale_x_date(NULL, breaks = presidential$start, date_labels = "'%y") +
    scale_y_continuous(breaks = seq(33, 44, 1)) +
    labs(
      title = "Term of Office",
      subtitle = "Presidents Eisenhower Through Obama",
      y = "Administration Number\n",
      x = "",
      color = "Political Party"
    )

# 28.4.2 Legend layout

base <- mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(colour = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right") # the default

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'

## 28.4.3 Replacing a scale

### Two alternatives for scaling

diamonds %>% ggplot(aes(carat, price)) +
  geom_bin2d()

diamonds %>% ggplot(aes(log10(carat), log10(price))) +
  geom_bin2d()

diamonds %>% ggplot(aes(carat, price)) +
  geom_bin2d() + 
  scale_x_log10() + 
  scale_y_log10()

### Colors for those challenged by color schemes

default_colors <- mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = drv))

friendly_colors <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_colour_brewer(palette = "Set1")

grid.arrange(default_colors, friendly_colors, nrow = 1)

### Combination of shape and color

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_colour_brewer(palette = "Set1")

### Presidential graph with colors by party

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(NULL, breaks = presidential$start, date_labels = "'%y") +
  scale_y_continuous(breaks = seq(33, 44, 1)) +
  scale_color_manual(values = c(Republican = "red", Democratic = "blue"))
  labs(
    title = "Term of Office",
    subtitle = "Presidents Eisenhower Through Obama",
    y = "Administration Number\n",
    x = "",
    color = "Political Party"
  )
  
### Viridis Colors
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
  
default_colors <- df %>%  ggplot(aes(x, y)) +
    geom_hex() +
    coord_fixed()
  
viridis_colors <- df %>% ggplot(aes(x, y)) +
    geom_hex() +
    viridis::scale_fill_viridis() +
    coord_fixed() 

grid.arrange(default_colors, viridis_colors, nrow = 1)
  
# 28.5 Zooming -----------------------------------------------------------------

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

mpg %>% ggplot(mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

mpg %>%
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>%
  ggplot(aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_smooth()

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")

suv %>% ggplot(aes(displ, hwy, colour = drv)) +
  geom_point()

compact %>% ggplot(aes(displ, hwy, colour = drv)) +
  geom_point()

### Homogeneous scales over multiple graphs

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_colour_discrete(limits = unique(mpg$drv))

suv_plot <- suv %>% ggplot(aes(displ, hwy, colour = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

compact_plot <- compact %>% ggplot(aes(displ, hwy, colour = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

grid.arrange(suv_plot, compact_plot, nrow = 1)

# 28.6 Themes ------------------------------------------------------------------

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_gray() +
  labs(
    title = "Default Gray"
  )

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  labs(
    title = "Black and White"
  )

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_classic() +
  labs(
    title = "Classic"
  )

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_dark() +
  labs(
    title = "Dark"
  )

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_light() +
  labs(
    title = "Light"
  )

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_linedraw() +
  labs(
    title = "LineDraw"
  )

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(
    title = "Minimal"
  )

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_void() +
  labs(
    title = "Void"
  )

# 28.7 Saving your plots -------------------------------------------------------

mpg %>% ggplot(aes(displ, hwy)) +
  geom_point()

ggsave("my-plot.pdf")
