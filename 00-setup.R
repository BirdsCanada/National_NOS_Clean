#Setup for NOS Clean

#Load libraries
library(tidyverse)
library(reshape)
library(sp)
library(sf)
library(naturecounts)
library(ggspatial)

# Create folders as necessary
if(!dir.exists("data")) dir.create("data")
if(!dir.exists("output")) dir.create("output")
if(!dir.exists("plots")) dir.create("plots")

out.dir <- paste("output/")
dat.dir <- paste("data/")
plot.dir<-paste("plots/")

#load species names list
sp.names<-meta_species_taxonomy()
sp.names<-sp.names %>% dplyr::select(species_id, english_name, scientific_name)

