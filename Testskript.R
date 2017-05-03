library(leaflet)
library(rgdal)
library(sp)
library(htmltools)

testfile <- "https://opentransportdata.swiss/dataset/695c7af6-d486-4cde-9bf0-a92fdd581a4e/resource/c1125c8d-13f9-4e38-bb51-9cc454382fbd/download/bfkoordgeo.csv"

daten <- read.csv(testfile, encoding = "UTF-8")

latitude <- daten$Latitude
longitude <- daten$Longitude



m <- leaflet() %>%
  setView(lng = 9.784532, lat = 46.648033, zoom = 10) %>%
  addTiles(group = "OpenStreetMap") %>%  # Add default OpenStreetMap map tiles
  addProviderTiles("Stamen.Terrain", group = "Toner by Stamen") 

for (i in latitude){
  print(paste(i))
}


m  # Print the map
