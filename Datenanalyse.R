# Preliminaries
rm(list=ls())             # clean the environment
options(scipen=6)         # display digits properly!! not the scientific version
options(digits.secs=6)    # use milliseconds in Date/Time data types
options(warning=FALSE)    # don't show warnings

# libraries
library(igraph)    # install.packages("igraph")
library(plyr)      # install.packages("plyr")
library(sp)        # install.packages("sp")
library(leaflet)   # install.packages("leaflet")
library(spatgraphs)# install.packages("spatgraphs")
library(ggplot2)
library(ggmap)


dataFolder <- "data"


# Merge Haltestelle.csv and Haltepunkt.csv with halt_id as unique identifier
# Read in the data, use encoding UTF-8

istdaten <- read.csv(file.path(dataFolder,"istDaten","2017-05-08istdaten.csv"), 
                     encoding = "UTF-8",stringsAsFactors = FALSE, sep = ";")

koordinaten <- read.csv(file.path(dataFolder,"stationsliste","bfkoordgeo.csv"), 
                     encoding = "UTF-8",stringsAsFactors = FALSE, sep = ",")


daten <- merge.data.frame(istdaten, koordinaten, by.x = "BPUIC", by.y = "StationID", all.x = TRUE, sort = FALSE)

#Rasterkarte als Hintergrund
CH <- get_map(location = c(median(koordinaten$Longitude, na.rm = TRUE), median(koordinaten$Latitude, na.rm = TRUE)), zoom = 7, source = 'google', color = 'bw')
map <- ggmap(CH)

#Small Multiples nach Kategorie cat
map <- map + geom_point(data = koordinaten, aes(Longitude, Latitude, color = Height)) + theme_minimal()
map

ggsave("plot.png", width = 15, height = 15)


daten.prog <- subset(daten, AB_PROGNOSE != "")

daten.prog$verspaetung <- (strptime(daten.prog$AB_PROGNOSE, format = '%d.%m.%Y %H:%M') - strptime(daten.prog$ABFAHRTSZEIT, format = '%d.%m.%Y %H:%M'))/60
