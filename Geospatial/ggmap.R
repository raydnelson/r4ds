library(ggmap)
library(tidyverse)

# Code from the ggmap github site

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)

get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% 
  ggmap()

`%notin%` <- function(lhs, rhs) !(lhs %in% rhs)

violent_crimes <- crime %>% 
  filter(
    offense %notin% c("auto theft", "theft", "burglary"),
    -95.39681 <= lon & lon <= -95.34188,
    29.73631 <= lat & lat <= 29.78400
  ) %>% 
  mutate(
    offense = fct_drop(offense),
    offense = fct_relevel(offense,
                          c("robbery", "aggravated assault",
                            "rape", "murder"))
  )

qmplot(lon, lat, data = violent_crimes,
       maptype = "toner-lite", color = I("red"))

qmplot(lon, lat, data = violent_crimes, maptype = "toner-lite",
       geom = "density2d", color = I("red"))

robberies <- violent_crimes %>% 
  filter(offense == "robbery")

qmplot(lon, lat, data = violent_crimes,
       geom = "blank",
       zoom = 14,
       maptype = "toner-background",
       darken = 0.7,
       legend = "topleft") +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon",
                  alpha = 0.3,
                  color = NA) +
  scale_fill_gradient2("Robbery\nPropensity",
                       low = "white",
                       mid = "yellow",
                       high = "red",
                       midpoint = 650)

qmplot(lon, lat, data = violent_crimes,
       maptype = "toner-background",
       color = offense) +
  facet_wrap(vars(offense))

# Google maps

register_google(key = "AIzaSyBj7F5KSag_NY51j2OqvjeGD-z9kajTNTw",
                write = TRUE)

get_googlemap("waco texas", zoom = 12) %>% 
  ggmap()

get_googlemap("waco texas", zoom = 12, maptype = "satellite") %>% 
  ggmap()
get_googlemap("waco texas", zoom = 12, maptype = "hybrid") %>% 
  ggmap()
get_googlemap("waco texas", zoom = 12, maptype = "roadmap") %>% 
  ggmap()

geocode("1301 S University Parks Dr, Waco, TX 76798")
revgeocode(c(lon = -97.1161, lat = 31.55098))

tibble(address = c("white house", "", "waco texas")) %>% 
  mutate_geocode(address)

trek_df <- trek("houston, texas", "waco, texas", structure = "route")

qmap("college stations, texas", zoom = 8) +
  geom_path(
    data = trek_df,
    aes(x = lon, y = lat),
    colour = "blue",
    size = 1.5,
    alpha = 0.5,
    lineend = "round"
  )

mapdist(c("houston, texas", "dallas"), "waco, texas")
