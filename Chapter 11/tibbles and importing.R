# Tibbles, data frames, importing, and exporting
# Initial: October 15, 2018
# Revision: October 15, 2018
# Ray Nelson

# Initial libraries
library(tidyverse)

# Tibbles
## Create tibble
x <- 1:10
y <- rnorm(10)
demo_tibble <- tibble(x, y)

## investigating the tibble
demo_tibble %>%
  ggplot(aes(x = x, y = y)) +
    geom_point()

# Information about the tibble
demo_tibble %>% class()
demo_tibble %>% is.tibble()
demo_tibble %>% is.character()

demo_tibble %>% summary()

# Viewing the tibble
demo_tibble
demo_tibble %>% View()

## Referencing the tibble
demo_tibble %>% str()
demo_tibble$x
demo_tibble$y

# Exporting
mtcars <- mtcars %>% mutate(car = rownames(mtcars))
write_sas(mtcars, "mtcars.sas7bdat")
write_dta(mtcars, "mtcars.dta")
write_csv(mtcars, "mtcars.csv")
write_sav(mtcars, "mtcars.sav")

# Importing
mtcars_csv <- read_csv("chapter 11/mtcars/mtcars.csv")
mtcars_sas <- read_sas("chapter 11/mtcars/mtcars.sas7bdat")
mtcars_dta <- read_dta("chapter 11/mtcars/mtcars.dta")
mtcars_sav <- read_sav("chapter 11/mtcars/mtcars.sav")

# Worksapce management
save.image("mtcars.RData")
ls()
rm(list = ls())
load("mtcars.rData")

# Parsing a vector
meg <- c("TRUE", "FALSE", "NA")
parse_logical(c("TRUE", "FALSE", "NA")) %>% str()
parse_integer(c("1", "2", "3")) %>% str()
parse_date(c("2010-01-01", "1979-10-14")) %>% str()

parse_number("$100")
parse_number("20%")
parse_number("It cost $123.45")

# Used in America
parse_number("$123,456,789")

# Parsing numbers from different locations

# Used in many parts of Europe
parse_number("123.456.789", locale = locale(grouping_mark = "."))

# Used in Switzerland
parse_number("123'456'789", locale = locale(grouping_mark = "'"))

# Characters
charToRaw("Hadley")

# Factors
fruit <- c("apple", "banana")
basket <- c("apple", "apple", "banana", "banana")
parse_factor(basket, levels = fruit)

# Dates, date-times, and times
parse_datetime("2010-10-01T2010")
parse_datetime("20101010")

parse_date("2010-10-01")

parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))

# Heat map
mtcars %>% 
  ggplot(aes(x = wt, y = mpg)) +
    # stat_density_2d(aes(fill = stat(level)), geom = "polygon")
    stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE) +
    geom_point(color = "red") +
    geom_smooth(color = "lightgreen", se = FALSE) +
  geom_smooth(method = "lm") +
    labs(title = "Relationship between Mileage and Car Weight",
         subtitle = "mtcars Tibble",
         x = "Weight in Thousands of Pounds",
         y = "Miles per Gallon")

