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


dataFolder <- "data"


# Merge Haltestelle.csv and Haltepunkt.csv with halt_id as unique identifier
# Read in the data, use encoding UTF-8

istdaten <- read.csv(file.path(dataFolder,"istDaten","2017-05-08istdaten.csv"), 
                     encoding = "UTF-8",stringsAsFactors = FALSE, sep = ";")

koordinaten <- read.csv(file.path(dataFolder,"fp2017jahresfahrplan","BFKOORD"), 
                     encoding = "UTF-8",stringsAsFactors = FALSE, sep = " ")
read.fwf(ff, widths = c(1,-2,3))