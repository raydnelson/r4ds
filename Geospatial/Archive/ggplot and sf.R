# R, sf, and ggplot2
# Initial: 03 Apr 2020
# Revision: 03 Apr 2020
# Ray Nelson

# Libraries
library(tidyverse)
library(sf)

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth",
                   "rnaturalearthdata"))

library(tidyverse)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world %>% 
  ggplot() +
  geom_sf() +
  labs(
    title = "World map",
    subtitle = paste0("(", world$sovereignt %>% unique() %>% length(),
                      " countries)"),
    x = "Longitude",
    y = "Latitude"
  )

world %>% 
  ggplot() +
  geom_sf(color = "black", fill = "lightblue") +
  labs(
    title = "World map",
    subtitle = paste0("(", world$sovereignt %>% unique() %>% length(),
                      " countries)"),
    x = "Longitude",
    y = "Latitude"
  )

world %>% 
  ggplot() %>% 

