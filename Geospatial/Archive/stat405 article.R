library(ggmap)
library(tidyverse)

murder <- crime %>% 
  filter(offense == "murder")

qmplot(data = murder, x = lon, y = lat, colour = 'red',
       size = 3, darken = 0.3)
