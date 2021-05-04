# ggplot and sf II
# Initial: 3 April 2020
# Revision: 3 April 2020
# Ray Nelson

# Libraries
library(tidyverse)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(ggspatial)
library(tools)
library(lwgeom)
library(googleway)
library(ggrepel)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Basic outline of the map
world %>% 
  ggplot() +
  geom_sf()

# Basic outline of the map but zoomed

world %>% 
  ggplot() +
  geom_sf() +
  coord_sf(xlim = c(-88, -78),
           ylim = c(24.5, 33),
           expand = FALSE)

# Now add points for two sites
(sites <- data.frame(longitude = c(-80.144005, -80.109),
                     latitude = c(26.479005, 26.83)))
world %>% 
  ggplot() +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude),
             size = 4,
             shape = 23,
             fill = "darkred") +
  coord_sf(xlim = c(-88, -78),
           ylim = c(24.5, 33),
           expand = FALSE)

# Let's use sf rather than geom_points

(sites <- st_as_sf(sites, coords = c("longitude", "latitude"),
                   crs = 4326, agr = "constant"))
world %>%
  ggplot() +
  geom_sf() +
  geom_sf(
    data = sites,
    size = 4,
    shape = 23,
    fill = "darkred"
  ) +
  coord_sf(xlim = c(-88,-78),
           ylim = c(24.5, 33),
           expand = FALSE)

# Add states

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- states$ID %>% as.character() %>% toTitleCase()
head(states)

world %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = states, fill = NA) +
  geom_text(data = states, aes(X, Y, label = ID), size = 5) +
  coord_sf(xlim = c(-88,-78),
           ylim = c(24.5, 33),
           expand = FALSE)

# Move the labels
states$nudge_y <- -1
states$nudge_y[states$ID == "Florida"] <- 0.5
states$nudge_y[states$ID == "South Carolina"] <- -1.5

world %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = states, fill = NA) +
  geom_label(data = states,
             aes(x = X, y = Y, label = ID),
             size = 5,
             fontface = "bold",
             nudge_y = states$nudge_y) +
  coord_sf(xlim = c(-88,-78),
           ylim = c(24.5, 33),
           expand = FALSE)

# Counties (polygon data)
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)

world %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(0.5)) +
  coord_sf(xlim = c(-88,-78),
           ylim = c(24.5, 33),
           expand = FALSE)

# Fill based on their area
world %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = counties, aes(fill = area)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = 0.4) +
  coord_sf(xlim = c(-88, -78),
           ylim = c(24.5, 33),
           expand = FALSE)

# Florida Cities data preparation
flcities <- data.frame(state = rep("Florida", 5),
                       city = c("Miami",
                                "Tampa",
                                "Orlando",
                                "Jacksonville",
                                "Sarasota"),
                       lat = c(25.7616798, 
                               27.950575,
                               28.5383355,
                               30.3321838,
                               27.3364347),
                       lng = c(-80.1917902,
                               -82.4571776,
                               -81.3792365,
                               -81.655651,
                               -82.5306527))

# Get the data for the cities from google
key <- "AIzaSyBj7F5KSag_NY51j2OqvjeGD-z9kajTNTw" # google maps key
flcities <- data.frame(state = rep("Florida", 5),
                       city = c("Miami", 
                                "Tampa",
                                "Orlando",
                                "Jacksonville",
                                "Sarasota"))
coords <- apply(flcities, 1, function(x) {
  google_geocode(address = paste(x["city"], x["state"], sep = ", "), 
                 key = key)
})
flcities <- cbind(flcities, do.call(rbind,
                                    lapply(coords, geocode_coordinates)))

(flcities <- st_as_sf(flcities, coords = c("lng", "lat"), remove = FALSE, 
                      crs = 4326, agr = "constant"))

# Simple Cities

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  geom_sf(data = flcities) +
  geom_text(data = flcities, aes(x = lng, y = lat, label = city), 
            size = 3.9, col = "black", fontface = "bold") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

# Cities using ggrepel

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  geom_sf(data = flcities) +
  geom_text_repel(data = flcities, aes(x = lng, y = lat, label = city), 
                  fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

# Final Graph

world %>%
  ggplot() +
  geom_sf(fill = "antiquewhite1") +
  geom_sf(data = counties, aes(fill = area)) +
  geom_sf(data = states, fill = NA) +
  geom_sf(
    data = sites,
    size = 4,
    shape = 23,
    fill = "darkred"
  ) +
  geom_sf(data = flcities) +
  geom_text_repel(
    data = flcities,
    aes(x = lng, y = lat, label = city),
    fontface = "bold",
    nudge_x = c(1, -1.5, 2, 2, -1),
    nudge_y = c(0.25, -0.25, 0.5, 0.5, -0.5)
  ) +
  geom_label(
    data = states,
    aes(X, Y, label = ID),
    size = 5,
    fontface = "bold",
    nudge_y = states$nudge_y
  ) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.75, "in"),
    pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  coord_sf(xlim = c(-88, -78),
           ylim = c(24.5, 33),
           expand = FALSE) +
  labs(
    title = "Observation Sites",
    subtitle = "(2 sites in Palm Beach County, Florida)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  )
