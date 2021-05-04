library(leaflet)
library(tidyverse)

load(url("http://govfinance.byu.edu/govfinance/classes/dataViz/lectures/workspaces/geospatial II.RData"))

lat <- rep(students$lat, 75) + rnorm(825, 0, 0.0035)
lng <- rep(students$lng, 75) + rnorm(825, 0, 0.0035)

covid <- tibble(lat, lng)

general_lat <- rep(40.28, 500) + rnorm(500, 0, 0.02)
general_lng <- rep(-111.7, 500) + rnorm(500, 0, 0.01)
general_covid <- tibble(lat = general_lat, lng = general_lng)

covid <- rbind(general_covid, covid)
  
covid %>% 
  leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.Positron) %>%
  addCircleMarkers(
    radius = 5,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.2
  )

rm(general_covid, general_lat, general_lng, lat, lng)

