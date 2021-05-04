library(tidyverse)
library(leaflet)
library(nycflights13)


# possible origins for first menu
possible_origins <- flights %>% 
  distinct(origin) %>% 
  left_join(airports, by = c("origin" = "faa")) %>% 
  select(origin, name)

# get the airport code for a given 
selected_origin <- possible_origins[2, 2] %>% 
  pull()

origin_code <- possible_origins %>% 
  filter(name == selected_origin) %>% 
  select(origin) %>% 
  pull()

# possible carriers

possible_carriers <- flights %>% 
  filter(origin == origin_code) %>%
  distinct(carrier) %>% 
  left_join(airlines)

selected_carrier <- possible_carriers[2, 2] %>% 
  pull()

carrier_code <- possible_carriers %>% 
  filter(name == selected_carrier) %>% 
  select(carrier) %>% 
  pull()

# Destinations
find_destinations <- function(origin_code, carrier_code) {
  destinations <- flights %>% 
    filter(origin == origin_code) %>% 
    filter(carrier == carrier_code) %>% 
    distinct(dest) %>% 
    add_row(dest = origin_code) %>% 
    pull()
  airports %>% 
    filter(faa %in% destinations) %>% 
    select(faa, lat, lon) 
}

(destination_location <- find_destinations(origin_code, carrier_code))


draw_map <- function(destination_locations) {
  destination_location %>%  
    leaflet() %>% 
    addProviderTiles(provider = providers$Esri.NatGeoWorldMap) %>% 
    addCircleMarkers(
      radius = 5,
      color = "red",
      stroke = FALSE,
      fillOpacity = 0.50
    )
}

draw_map(destinations)
