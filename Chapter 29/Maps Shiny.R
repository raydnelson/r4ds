# Function Practicum
# Initial: 10 March 2021
# Revision: 10 March 2021
# Ray Nelson

# Libraries
library(tidyverse)
library(lubridate)
library(nycflights13)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# airport and destination map

# Test Parameters
# nyc_airport <- "JFK"
# airline <- "DL"

# Find the destinations of an airline from a given airport on a given airline

find_destinations <- function(nyc_airport, airline) {
  destinations <- flights %>% 
    filter(origin == nyc_airport) %>% 
    filter(carrier == airline) %>% 
    distinct(dest) %>% 
    add_row(dest = nyc_airport)
  airports %>% 
    semi_join(destinations, by = c("faa" = "dest")) %>% 
    select(name, lat, lon) 
}

destinations <- find_destinations("JFK", "DL")

# Function to Create Title
create_title <- function(nyc_airport, airline) {
  # Create the Text for the title
  title <- paste("NYC Airport:", nyc_airport, "Airline:", airline)
  
  # CSS for formatting title
  tag.map.title <- tags$style(HTML("
    .leaflet-control.map-title { 
      transform: translate(-50%,20%);
      position: fixed !important;
      left: 50%;
      text-align: center;
      padding-left: 10px; 
      padding-right: 10px; 
      background: rgba(255,255,255,0.75);
      font-weight: bold;
      font-size: 28px;
    }
  "))
  
  # Create the HTML for the title
  tags$div(
    tag.map.title, HTML(title)
  )
}

# Test the map title function
create_title("JFK", "DL")

# Use leaflet to create a map from the results of destinations and map_title

draw_map <- function(destinations, map_title) {
  destinations %>%  
    leaflet() %>% 
    addProviderTiles(provider = providers$Esri.NatGeoWorldMap) %>% 
    addCircleMarkers(
      radius = 5,
      color = "red",
      stroke = FALSE,
      fillOpacity = 0.50
    ) %>%
    addControl(map_title, position = "topleft", className="map-title")
}

draw_map(destinations)

# Function puts the three parts of the map together.

destinations_map <- function(nyc_airport, airline) {
  destinations <- find_destinations(nyc_airport, airline)
  title_for_map <- create_title(nyc_airport, airline)
  draw_map(destinations, title_for_map)
}

# Execution of the function
destinations_map("JFK", "AA")
  