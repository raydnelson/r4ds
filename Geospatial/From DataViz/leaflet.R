# Leaflet for R Tutorial
# Initial: 3 Apr 2020
# Revision: 3 Apr 2020
# Ray Nelson

# Libraries
library(leaflet)
library(tidyverse)
library(sp)
library(maps)
library(sf)
library(RJSONIO)
library(rgeos)
library(R6)
library(htmltools)

## Parameters for leaflet
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

## Coordinates for Tanner Building
latitude <- 40.2504
longitude <- -111.6525

(m <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = longitude,
             lat = latitude,
             popup = "Marriott School of Business")
)

## Function to create a map
leaflet_map <- function(longitude, latitude, popup){
  require(leaflet)
  leaflet() %>% 
    addTiles() %>% 
    addMarkers(lng = longitude, lat = latitude, popup = popup)
}

## Coordinates for Nelson Home
## Latitude: 40.268831
## longitude: -111.644243

leaflet_map(longitude = -111.644243,
            latitude = 40.268831,
            popup = "Mooki's Hideout")


# add some circles to a map

df = data.frame(Lat = 1:10, Long = rnorm(10))
df %>%
  leaflet() %>% 
  addCircles()

df %>% 
  leaflet() %>% 
  addCircles(lng = ~Long, lat = ~Lat)

leaflet() %>% 
  addCircles(data = df)

leaflet() %>% 
  addCircles(data = df, lat = ~Lat, lng = ~Long)

# Examples that use the library sp

Sr1 = Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
Sr2 = Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
Sr3 = Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
Sr4 = Polygon(cbind(c(5, 6, 6, 5, 5), c(4, 4, 3, 3, 4)), hole = TRUE)
Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr4, Sr3), "s3/4")
SpP = SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
leaflet(height = "300px") %>% addPolygons(data = SpP)

# Examples that use the library maps

mapStates = map("state", fill = TRUE, plot = FALSE)
mapStates %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(fillColor = topo.colors(10, alpha = NULL),
              stroke = FALSE)

# The formula interface

m <- leaflet() %>%
  addTiles()
m

df  <-  data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)

m <- df %>% 
  leaflet() %>% 
  addTiles()
m %>% 
  addCircleMarkers(radius = ~size,
                   color = ~color,
                   fill = FALSE)
m %>% 
  addCircleMarkers(radius = runif(100, 4, 10),
                   color = "red")

# Using Basemaps
m <- leaflet() %>%
  setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% 
  addTiles()

home <- leaflet() %>%
  setView(lng = -111.644243, lat = 40.26881, zoom = 14)
home %>% 
  addTiles() %>% 
  addMarkers(lng = -111.644243, lat = 40.26881,
             popup = "Mooki's Hideout")


# Third-Party Tiles

m %>% addProviderTiles(provider = providers$Stamen.Toner)
m %>% addProviderTiles(provider = providers$CartoDB.Positron)

home %>%
  addProviderTiles(provider = providers$Stamen.Toner) %>% 
  addMarkers(lng = -111.644243, lat = 40.26881,
             popup = "Mooki's Hideout")

home %>%
  addProviderTiles(provider = providers$CartoDB.Positron) %>% 
  addMarkers(lng = -111.644243, lat = 40.26881,
             popup = "Mooki's Hideout")

# WMS Tiles

leaflet() %>% 
  addTiles() %>% 
  setView(-93.65, 42.0285, zoom = 4) %>% 
  addWMSTiles(
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    layers = "nexrad-n0r-900913",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "Weather data © 2012 IEM Nexrad"
  )

# Combining Tile Layers
m %>% 
  addProviderTiles(providers$MtbMap) %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.35)) %>% 
  addProviderTiles(providers$Stamen.TonerLabels)

# Markers
data(quakes)

quakes %>% 
  head(20) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~long, ~lat,
             popup = ~as.character(mag),
             label = ~as.character(mag))

# Customizing Marker Icons

greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38,
  iconHeight = 95,
  iconAnchorX = 22,
  iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50,
  shadowHeight = 64,
  shadowAnchorX = 4,
  shadowAnchorY = 62
)

quakes %>% 
  head(4) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~long, ~lat, icon = greenLeafIcon)

# Multiple colors

quakes1 <- quakes %>% head(10)

leafIcons <- icons(
  iconUrl = ifelse(
    quakes1$mag < 4.6,
    "http://leafletjs.com/examples/custom-icons/leaf-green.png",
    "http://leafletjs.com/examples/custom-icons/leaf-red.png"
  ),
  iconWidth = 38,
  iconHeight = 95,
  iconAnchorX = 22,
  iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50,
  shadowHeight = 64,
  shadowAnchorX = 4,
  shadowAnchorY = 62
)

quakes1 %>% 
  leaflet() %>% 
  addTiles() %>%
  addMarkers(~long, ~lat, icon = leafIcons)

# Variety of icons

oceanIcons <- iconList(
  ship = makeIcon("ferry-18.png", "ferry=18@2x.pgn, 18, 18"),
  pirate = makeIcon("danger-24.png", "danger-24@2x.png", 24, 24)
)

# Some fake data

df <- sp::SpatialPointsDataFrame(cbind(
  (runif(20) - 0.5) * 10 - 90.620130, # longitude
  (runif(20) - 0.5) * 3.8 + 25.638077 # Latitude
   ),
    data.frame(type = factor(
      ifelse(runif(20) > 0.75, "pirate", "ship"),
      c("ship", "pirate")
    ))
  )

df %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers()


# first 20 quakes
df.20 <- quakes[1:20,]

getColor <- function(quakes) {
  sapply(quakes$mag, function(mag) {
    if(mag <= 4) {
      "green"
    } else if(mag <= 5) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)

leaflet(df.20) %>% addTiles() %>%
  addAwesomeMarkers(~long, ~lat, icon=icons, label=~as.character(mag))

# Popups

content <- paste(
  "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
  "606 5th Ave. S",
  "Seattle, WA 98138",
  sep = "<br/>"
)

leaflet() %>%
  addTiles() %>%  
  addPopups(lng = -122.327298, lat = 47.597131,
            content,
            options = popupOptions(closeButton = FALSE))

# Labels appear on mouse click

df <- read.csv(textConnection(
  "Name, Lat, Long
  Samurai Noodle, 47.597131, -122.327298
  Kukai Ramen, 47.6154, -122.327157
  Tsukushinbo, 47.59987, -122.326726"
))

df %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name))

# Change Text Size and text Only and also a custom CSS
leaflet() %>% addTiles() %>% setView(-118.456554, 34.09, 13) %>%
  addMarkers(
    lng = -118.456554, lat = 34.105,
    label = "Default Label",
    labelOptions = labelOptions(noHide = T)) %>%
  addMarkers(
    lng = -118.456554, lat = 34.095,
    label = "Label w/o surrounding box",
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>%
  addMarkers(
    lng = -118.456554, lat = 34.085,
    label = "label w/ textsize 15px",
    labelOptions = labelOptions(noHide = T, textsize = "15px")) %>%
  addMarkers(
    lng = -118.456554, lat = 34.075,
    label = "Label w/ custom CSS style",
    labelOptions = labelOptions(noHide = T, direction = "bottom",
                                style = list(
                                  "color" = "red",
                                  "font-family" = "serif",
                                  "font-style" = "italic",
                                  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                  "font-size" = "12px",
                                  "border-color" = "rgba(0,0,0,0.5)"
                                )))

# Lines and Shapes
library(rgdal)
states <- readOGR("shp/cb_2013_us_state_20m.shp",
                  layer = "cb_2013_us_state_20m",
                  GDAL1_integer64_policy = TRUE)

states <- readOGR("shp/cb_2013_us_state_20m.shp",
                  layer = "cb_2013_us_state_20m",
                  GDAL1_integer64_policy = TRUE)



neStates <- subset(states)
