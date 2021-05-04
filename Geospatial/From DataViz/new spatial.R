library(leaflet)
library(sp)
library(maps)
library(tidyverse)

df <- data.frame(lat = 1:10, long = rnorm(10))

df %>% 
  leaflet() %>% 
  addCircles()

Sr1 <- Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
Sr2 <- Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
Sr3 <- Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
Sr4 <- Polygon(cbind(c(5, 6, 6, 5, 5), c(4, 4, 3, 3, 4)), hole = TRUE)
Srs1 <- Polygons(list(Sr1), "s1")
Srs2 <- Polygons(list(Sr2), "s2")
Srs3 <- Polygons(list(Sr4, Sr3), "s3/4")
SpP <- SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
leaflet(height = "300px") %>% addPolygons(data = SpP)

map_states <- map("state", fill = TRUE, plot = FALSE)
us %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

m <- leaflet() %>% 
  addTiles()
m
df <- data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m <- leaflet(df) %>% 
  addTiles()
m
m %>% 
  addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)

m %>% 
  addCircleMarkers(radius = runif(100, 4, 10), color = "red")

m <- leaflet() %>% 
  setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% 
  addTiles()
m %>% 
  addProviderTiles(providers$Stamen.Toner)
m %>% 
  addProviderTiles(providers$CartoDB.Positron)
m %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap)


leaflet() %>%
  addTiles() %>%
  setView(-93.65, 42.0285, zoom = 4) %>%
  addWMSTiles(
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    layers = "nexrad-n0r-900913",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "Weather data © 2012 IEM Nexrad"
  )

m %>%
  addProviderTiles(providers$MtbMap) %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.35)) %>%
  addProviderTiles(providers$Stamen.TonerLabels)

quakes %>% 
  head(20) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))


greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

leaflet(data = quakes[1:4,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = greenLeafIcon)

quakes1 <- quakes %>% 
  head(10)

leafIcons <- icons(
  iconUrl = ifelse(quakes1$mag < 4.6,
                   "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                   "http://leafletjs.com/examples/custom-icons/leaf-red.png"
  ),
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

quakes1 %>% 
  leaflet() %>% 
  addTiles() %>%
  addMarkers(~long, ~lat, icon = leafIcons)


# Make a list of icons. We'll index into it based on name.
oceanIcons <- iconList(
  ship = makeIcon("ferry-18.png", "ferry-18@2x.png", 18, 18),
  pirate = makeIcon("danger-24.png", "danger-24@2x.png", 24, 24)
)

# Some fake data
df <- sp::SpatialPointsDataFrame(
  cbind(
    (runif(20) - .5) * 10 - 90.620130,  # lng
    (runif(20) - .5) * 3.8 + 25.638077  # lat
  ),
  data.frame(type = factor(
    ifelse(runif(20) > 0.75, "pirate", "ship"),
    c("ship", "pirate")
  ))
)

leaflet(df) %>% addTiles() %>%
  # Select from oceanIcons based on df$type
  addMarkers(icon = ~oceanIcons[type])


quakes %>% 
  leaflet() %>% 
  addTiles() %>%
  addMarkers(
  clusterOptions = markerClusterOptions()
)

df %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers()

pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))

df %>% 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(type == "ship", 6, 10),
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 0.5
  )

quakes %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(
    clusterOptions = markerClusterOptions()
  )

m <- states %>% 
  leaflet() %>% 
  setView(-96, 37.8, 4) %>% 
  addTiles()

m %>% 
  addPolygons()


bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)


labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  states$name, states$density
) %>% lapply(htmltools::HTML)

states %>% 
  leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright")


nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
ggplot(nc) +
  geom_sf(aes(fill = AREA))

library(maps)

world <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))
us <- sf::st_as_sf(maps::map('state', plot = FALSE, fill = TRUE))
utah <- sf::st_as_sf(maps::map('county', 'utah', plot = FALSE, fill = TRUE))

utah %>% 
  ggplot(aes(fill = ID)) +
  geom_sf(alpha = 0.5, show.legend = FALSE)

us %>% 
  ggplot(aes(fill = ID)) +
  geom_sf(alpha = 0.5, show.legend = FALSE)

world %>% 
  ggplot(aes(fill = ID)) +
  geom_sf(alpha = 0.5, show.legend = FALSE)

(TNRB <- leaflet() %>% 
    addTiles() %>% 
    addMarkers(lng = -71.0589,
               lat = 42.3601,
               popup = "Marriott School of Business")
)

m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()


(TNRB <- leaflet() %>% 
    addTiles() %>% 
    addMarkers(lat = 40.2504,
               lng = -111.6525,
               popup = "Marriott School of Business")
)
