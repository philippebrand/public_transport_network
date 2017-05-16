# Preliminaries
rm(list=ls())             # clean the environment
options(scipen=6)         # display digits properly!! not the scientific version
options(digits.secs=6)    # use milliseconds in Date/Time data types
options(warning=FALSE)    # don't show warnings

install.packages("igraph")
install.packages("spatgraphs")

# libraries
library(igraph)    # install.packages("igraph")
library(plyr)      # install.packages("plyr")
library(sp)        # install.packages("sp")
library(leaflet)   # install.packages("leaflet")
library(spatgraphs)# install.packages("spatgraphs")



dataFolder <- "data"


# Merge Haltestelle.csv and Haltepunkt.csv with halt_id as unique identifier
# Read in the data, use encoding UTF-8

location <- read.csv(file.path(dataFolder,"ZVV_Performance","haltepunkt.csv"), 
                     encoding = "UTF-8",stringsAsFactors = FALSE)
location2  <- read.csv(file.path(dataFolder,"ZVV_Performance","haltestelle.csv"), 
                       encoding = "UTF-8",stringsAsFactors = FALSE)


# Coordinates are denoted as 47,44. R requires a "." to read the values properly. 
# Replace "," with "." in the GPS columns with ?gsub and convert to numeric
location$Latitude <- as.numeric(gsub(",",".",location$GPS_Latitude))
location$Longitude <- as.numeric(gsub(",",".",location$GPS_Longitude))
# remove from first column name this weird character "X.U.FEFF.halt_id"
names(location)[1]<- "halt_punkt_id"
names(location2)[1] <- "halt_id"


# merge stations with location, by using the "halt_id" as unique identifier
location <- merge(location2,location,by="halt_id")
location <- location[names(location) %in% c("halt_kurz","Latitude","Longitude")]
location <- location[!is.na(location$Latitude),]
location <- location[!duplicated(location$halt_kurz),]
rm(location2)


station  <- read.csv(file.path(dataFolder,"ZVV_Passenger","Haltestellen.csv"), 
                     encoding = "UTF-8",sep=";",stringsAsFactors = FALSE)
names(station)[1] <- "halt_id" # remove weird character "X.U.FEFF.halt_id"


# Merge the station table with the location table containing station id and coordinates 
station <- merge(station,location,by.x="Haltestellenkurzname",by.y="halt_kurz")

# Read in the passenger data
passenger<- read.csv(file.path(dataFolder,"ZVV_Passenger","reisende.csv"), 
                     encoding = "UTF-8",sep = ";",stringsAsFactors = FALSE)

# Select the tram lines 1 - 17 (without 1 and 16)
tram <- passenger[passenger$Linienname %in% 1:17,]
tramstation <- station[station$halt_id %in% unique(tram$Haltestellen_Id),]


# edgelist by Haltestellen_Id Nach_Hst_Id with id ID_Abschnitt
# first 2 columns are from, to 
gdata <- ddply(tram,c("ID_Abschnitt","Linien_Id"),function(d){
  # select unique Linien_Id, Linienname, Sequenz, Haltestellen_Id, Nach_Hst_Id,ID_Abschnitt, Distanz
  # the first two columns should be from/to edges.
  col <-  c("Haltestellen_Id","Nach_Hst_Id","ID_Abschnitt","Linien_Id","Linienname","Sequenz","Distanz")
  r<- d[1,col]
  # calculate some measures for each edge
  n<-d[,"Anzahl_Messungen"]
  on<- d[,"Einsteiger"]
  off<-d[,"Aussteiger"]
  occ<-d[,"Besetzung"]
  r$on <- sum(on*n,na.rm=TRUE) # total passenger on
  r$mean_on <- sum(on*n,na.rm=TRUE)/sum(n,na.rm=TRUE) # average on
  r$off <- sum(on*n,na.rm=TRUE) # total passenger off
  r$mean_off <- sum(on*n,na.rm=TRUE)/sum(n,na.rm=TRUE) # average off
  r$occupancy <- sum(occ*n,na.rm=TRUE) # total occupancy
  r$mean_occ <- sum(occ*n,na.rm=TRUE)/sum(n,na.rm=TRUE) # average occupancy
  return(r)
})

# remove edges with NA from gdata
gdata <- gdata[!is.na(gdata$Nach_Hst_Id),]
head(gdata)

# create graph from edgelist
# the first two columns should form a "symbolic" edge
g <- graph_from_data_frame(gdata, directed=TRUE)

# add vertex data to the graph
roworder <- match(V(g)$name, as.character(tramstation$halt_id))
V(g)$latitude  <- tramstation[roworder,"Latitude"]
V(g)$longitude <- tramstation[roworder,"Longitude"]
V(g)$name_long <- as.character(tramstation[roworder,"Haltestellenlangname"])
V(g)$name <- as.character(tramstation[roworder,"Haltestellenkurzname"])
V(g)$size <- 6 

# add the degree of the vertex as an attribute 
V(g)$degree <- degree(g, mode="all")

# Tram line names
E(g)$label <- E(g)$Linienname
# Generate colors base on tram line:
# Source: https://de.wikipedia.org/wiki/Strassenbahn_Z%C3%BCrich
lineNumbers <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,17)
lineColors <- c("#D8232A", "#009F4A", "#3E4085", "#855B37", "#DA9F4F", "#191919","#86CD16", "#3E4085","#DA3987","#009F4A", "#7ACAD4", "#FBD01F", "#00A4DB", "#D8232A", "#933555")
roworder  <- match(E(g)$Linienname, as.character(lineNumbers))
E(g)$color<- lineColors[roworder]

plot.igraph(g,layout=layout_with_fr(g), vertex.label.color="black",vertex.frame.color    =NA,vertex.color="black", vertex.size =log10(V(g)$degree)*1.2+1,vertex.label.dist=0.3,vertex.label.degree=-pi/3, vertex.label=V(g)$name,vertex.label.font=2,vertex.label.cex   =0.5,edge.arrow.size=0, edge.curved=0,edge.label=NA)

coords <- matrix(c(V(g)$longitude,V(g)$latitude),ncol=2)
coords[is.na(coords)] <- 0
geolayout <- layout.norm(coords)
# remove loops
g <- simplify(g, remove.multiple = F, remove.loops = T) 

plot.igraph(g,layout=geolayout, vertex.label.color="black",vertex.frame.color    =NA,vertex.color="black", vertex.size =log10(V(g)$degree)*1.2+1,vertex.label.dist=0.3,vertex.label.degree=-pi/3, vertex.label=V(g)$name,vertex.label.font=2,vertex.label.cex   =0.5,edge.arrow.size=0, edge.curved=0,edge.label=NA)

# Extract data from graph
gg <- get.data.frame(g, "both")

# Convert graph vertices to SP points:
vert <- gg$vertices
coordinates(vert) <- ~longitude+latitude

# Convert edges to SP lines
edges <- gg$edges
edges <- lapply(1:nrow(edges), function(i) {
  as(rbind(vert[vert$name == edges[i, "from"], ], vert[vert$name == edges[i, "to"], ]), "SpatialLines")
})
# add unique id to every edge
for (i in seq_along(edges)) {edges[[i]] <- spChFIDs(edges[[i]], as.character(i))}
edges <- do.call(rbind, edges)
edgesdf <- SpatialLinesDataFrame(edges,data.frame(gg$edges), match.ID = TRUE)


popup <- paste(vert$name_long,"<br/>Degree:",vert$degree)
library(leaflet)
leaflet(vert) %>% addProviderTiles("Esri.WorldTopoMap", group = "Terrain")  %>% 
  addPolylines(data = edges,color="steelblue",weight=edgesdf$mean_occ/100*10+1)%>%
  addCircleMarkers(data=vert,radius=~ifelse(degree > 20, 6, 2),color = "gray60",stroke = FALSE, fillOpacity = 0.8, popup = popup)

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

gun <- as.undirected(g)
V(gun)$degree <- degree(gun, mode="all")
hist(V(gun)$degree, breaks=1:max(V(gun)$degree), main="Histogram of node degree (ZVV Tram undirected)")

# add the degree of the vertex as an attribute 
V(mstg)$degree <- degree(mstg, mode="all")
hist(V(mstg)$degree, breaks=1:max(V(mstg)$degree), main="Histogram of node degree (MST)")

deg.dist <- degree_distribution(gun, cumulative=T, mode="all")
plot( x=0:max(V(gun)$degree), y=1-deg.dist, pch=19, cex=1.2, col="orange", xlab="Degree", ylab="Cumulative Frequency  (ZVV Tram undirected)")


deg.dist <- degree_distribution(mstg, cumulative=T, mode="all")
plot( x=0:max(V(mstg)$degree), y=1-deg.dist, pch=19, cex=1.2, col="orange", xlab="Degree", ylab="Cumulative Frequency (MST)")

V(g)$betweenness <- betweenness(g)

?betweenness

v<-get.data.frame(g, "vertices")
d <- as.data.frame(v[order(v$betweenness,decreasing = T),])
knitr::kable(head(d[,c(1,4,6,7)],n=5), digits = 2, caption = "ZVV stations - betweeness")

vertex.connectivity(as.undirected(g))
vertex.connectivity(as.undirected(mstg))
diameter(g, directed = TRUE)
diameter(mstg, directed = TRUE)

V(g)$closeness <- closeness(g, mode="all")

v<-get.data.frame(g, "vertices")
d <- as.data.frame(v[order(v$closeness,decreasing = T),])
knitr::kable(head(d[,c(1,4,6,7,8)],n=5), digits =6, caption = "ZVV stations - closeness")

wc<-cluster_walktrap(g, weights = E(g)$Distanz, steps = 15, merges = TRUE, modularity = TRUE, membership = TRUE)

plot(wc,g,layout=geolayout, vertex.label.color="black",vertex.frame.color    =NA,vertex.color="black", vertex.size =log10(V(g)$degree)*1.2+1,vertex.label.dist=0.3,vertex.label.degree=-pi/3, vertex.label=V(g)$name,vertex.label.font=2,vertex.label.cex   =0.5,edge.arrow.size=0, edge.curved=0,edge.label=NA)






