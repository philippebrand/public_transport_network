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
library(rgdal)


dataFolder <- "data"


# Merge Haltestelle.csv and Haltepunkt.csv with halt_id as unique identifier
# Read in the data, use encoding UTF-8

istdaten <- read.csv(file.path(dataFolder,"istDaten","2017-05-08istdaten.csv"), 
                     encoding = "UTF-8",stringsAsFactors = FALSE, sep = ";")

koordinaten <- read.csv(file.path(dataFolder,"stationsliste","bfkoordgeo.csv"), 
                     encoding = "UTF-8",stringsAsFactors = FALSE, sep = ",")

solldaten <- read.csv(file.path(dataFolder,"fp2017jahresfahrplan","FPLAN"), 
                        encoding = "UTF-8",stringsAsFactors = FALSE, sep = "", row.names = FALSE)


daten <- merge.data.frame(istdaten, koordinaten, by.x = "BPUIC", by.y = "StationID", all.x = TRUE, sort = FALSE)

#Rasterkarte als Hintergrund
CH <- get_map(location = c(median(koordinaten$Longitude, na.rm = TRUE), median(koordinaten$Latitude, na.rm = TRUE)), zoom = 7, source = 'google', color = 'bw')
map <- ggmap(CH)

#Small Multiples nach Kategorie cat
map <- map + geom_point(data = koordinaten, aes(Longitude, Latitude, color = Height)) + theme_minimal()
map

ggsave("plot.png", width = 15, height = 15)


daten.prog <- subset(daten, AB_PROGNOSE != "")

daten.prog$verspaetung <- (strptime(daten.prog$AB_PROGNOSE, format = '%d.%m.%Y %H:%M') - strptime(daten.prog$ABFAHRTSZEIT, format = '%d.%m.%Y %H:%M'))

linien <- c(unique(daten.prog$LINIEN_ID))


linie.vec <- c()
from.vec <- c()
to.vec <- c()
versp.vec <- c()

for(i in c(linien)){
  temp <- (subset(daten.prog, LINIEN_ID == i))
  from <- min((subset(temp, ABFAHRTSZEIT == min(temp$ABFAHRTSZEIT))$BPUIC))
  to <- max((subset(temp, ANKUNFTSZEIT == max(temp$ANKUNFTSZEIT))$BPUIC))
  versp <- (mean(temp$verspaetung))
  linie.vec <- append(linie.vec, i)
  from.vec <- append(from.vec, from)
  to.vec <- append(to.vec, to)
  versp.vec <- append(versp.vec, as.integer(versp))
}

edges <- data.frame(linie.vec, from.vec, to.vec, versp.vec, stringsAsFactors = FALSE)

edges.sort <- data.frame(edges$from.vec, edges$to.vec, edges$linie.vec, edges$versp.vec)

g <- graph_from_data_frame(edges.sort)


edges.sort$edges.linie.vec
# add vertex data to the graph
roworder <- match(V(g)$name, as.character(koordinaten$StationID))
V(g)$latitude  <- koordinaten[roworder,"Latitude"]
V(g)$longitude <- koordinaten[roworder,"Longitude"]
V(g)$name_long <- as.character(koordinaten[roworder,"Remark"])

V(g)$degree <- degree(g, mode="all")

gg <- get.data.frame(g, "both")

# Convert graph vertices to SP points:
vert <- gg$vertices
coordinates(vert) <- ~longitude+latitude



gg$edges
edges.versp.vec

# Convert edges to SP lines
edges <- gg$edges
edges
edges <- lapply(1:nrow(edges), function(i) {
  as(rbind(vert[vert$name == edges[i, "from"], ], vert[vert$name == edges[i, "to"], ]), "SpatialLines")
})


# add unique id to every edge
for (i in seq_along(edges)) {edges[[i]] <- spChFIDs(edges[[i]], as.character(i))}
edges <- do.call(rbind, edges)
edgesdf <- SpatialLinesDataFrame(edges,data.frame(gg$edges), match.ID = TRUE)

edgesdf

pal <- colorNumeric(palette = "Reds", domain = c(3,5,10,20,max(edgesdf$edges.versp.vec)/60), na.color = 'lightblue')
?colorNumeric


?colorRamplightblue

popup <- paste(vert$name_long,"<br/>Degree:",vert$degree)
popupedge <- paste("Linie ", edgesdf$edges.linie.vec,"<br/>Verspätung:",round(edgesdf$edges.versp.vec/60,1), " min")
library(leaflet)
leaflet(vert) %>% addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Terrain")  %>% 
  addPolylines(data = edgesdf,color=~pal(edges.versp.vec/60),weight=~ifelse(edges.versp.vec/60 < 3, 2, 5), popup = popupedge)%>%
  addCircleMarkers(data=vert,radius=~ifelse(degree > 50, 6, 2),color = "gray60",stroke = FALSE, fillOpacity = 0.8, popup = popup)



?addProviderTiles




require(spatgraphs)
coords <- coordinates(vert)
knn2 <- spatgraph(coords,"knn",par=2)
mst <- spatgraph(coords,"MST")
plot(knn2,coords)
plot(mst,coords)

mstg <- graph_from_adj_list(mst$edges)
V(mstg)$name  <- rownames(coordinates(vert))
V(mstg)$latitude  <- coordinates(vert)[,2]
V(mstg)$longitude <- coordinates(vert)[,1]

# Convert edges to SP lines
# Extract data from graph
mstE <- get.data.frame(mstg, "edges")
mstV <- get.data.frame(mstg, "vertices")
coordinates(mstV) <- ~longitude+latitude
mstE <- lapply(1:nrow(mstE), function(i) {
  as(rbind(mstV[mstV$name == mstE[i, "from"], ], mstV[mstV$name == mstE[i, "to"], ]), "SpatialLines")
})
# add unique id to every edge
for (i in seq_along(mstE)) {mstE[[i]] <- spChFIDs(mstE[[i]], as.character(i))}
mstE <- do.call(rbind, mstE)
mstEdf <- SpatialLinesDataFrame(mstE,data.frame(get.data.frame(mstg, "edges")), match.ID = TRUE)


leaflet(vert) %>% addProviderTiles("Esri.WorldTopoMap", group = "Terrain")  %>% 
  addPolylines(data = mstEdf,color="red",weight=2, fillOpacity = 0.8,group = "MST")%>%
  addPolylines(data = edges,color="steelblue",weight=2, fillOpacity = 0.8,group = "ZVV Tram")%>%
  addCircleMarkers(data=vert,radius=2,color = "gray60",stroke = FALSE, fillOpacity = 0.8,group = "ZVV Tram" )%>%
  addLayersControl(
    baseGroups = c("Terrain"),
    overlayGroups =   c("ZVV Tram","MST"),
    options = layersControlOptions(collapsed = TRUE)
  )


