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
library(ggspatial)

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
  ggplot() + 
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

world %>% 
  ggplot() +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

world %>% 
  ggplot() +
  geom_sf() +
  coord_sf(crs = "+init=epsg:3035")

world %>% 
  ggplot() +
  geom_sf() +
  coord_sf(crs = st_crs(3035))

world %>% 
  ggplot() +
  geom_sf() +
  coord_sf(xlim = c(-102.15, -74.12),
           ylim = c(7.65, 33.97),
           expand = FALSE)

world %>% 
  ggplot() +
  geom_sf() + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))
  
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

world %>%
  ggplot() +
  geom_sf(fill = "antiquewhite") +
  geom_text(
    data = world_points,
    aes(x = X, y = Y, label = name),
    color =  "darkblue",
    fontface =  "bold",
    check_overlap = FALSE
  ) +
  annotate(
    geom =  "text",
    x = -90,
    y = 26,
    label =  "Gulf of Mexico",
    fontface =  "italic",
    color =  "grey22",
    size = 6) +
  annotation_scale(location =  "bl", width_hint = 0.5) +
  annotation_north_arrow(
    location =  "bl",
    which_north =  "true",
    pad_x = unit(0.75,  "in"),
    pad_y = unit(0.5,  "in"),
    style = north_arrow_fancy_orienteering) +
  coord_sf(
    xlim = c(-102.15,-74.12),
    ylim = c(7.65, 33.97),
    expand = FALSE) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Map of the Gulf of Mexico and the Caribbean Sea") +
  theme(
    panel.grid.major = element_line(
      color = gray(.5),
      linetype =  "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill ="aliceblue")
  )
