### ---- Mare Vivu Représentation ----

# Initialization ----

## Clean up and working directory -----
rm(list = ls())
setwd("C:/Users/aubin/Documents/Travail/Stareso/MareVivu/RIPARU")

## Library imports -----
# Data tidying 
library(dplyr)
library(tidyr)
library(tibble)

# Plotting
library(ggplot2)
library(readxl)

# Maps
library(mapview)
library(sf)
library(leafpop)

# Shiny
library(shiny)
library(bslib)


## Data imports ----

microplastics <- readRDS("Analyses/OutputData/Riparu_microplastics.rds")
microplastics_total <- readRDS("Analyses/OutputData/Riparu_microplastics_total.rds")
macroplastics <- readRDS("Analyses/OutputData/Riparu_macroplastics.rds")
typologie_sites <- readRDS("Analyses/OutputData/Riparu_typologie_sites.rds")


# Transforming datasets to sf format for map plotting

# Macroplastics
macroplastics_sf <- macroplastics %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  # Convert to sf object

# Typologie sites
st_segment = function(r){st_linestring(t(matrix(unlist(r), 2, 2)))}


typologie_sites$geom = st_sfc(sapply(1:nrow(typologie_sites), 
                                     function(i){st_segment(typologie_sites[i,1:4])}, simplify = FALSE))

typologie_sites_sf <- typologie_sites %>%
  st_sf(crs = 4326)  # Convert to sf object

# Microplastics

# Do the same for microplastics on the base of typologie site, but this time the geometry is a polygon defined by the
# points a, b, c, d at the coordinates Latitude_a, Longitude_a, Latitude_b, Longitude_b, that are at column 8 to 15

st_polygon_better = function(r){
  m <- matrix(unlist(r), 4, 2)
  # Change order to a, c, d, b
  m <- m[c(1,3,4,2),]
  m <- m[, c(2,1)] # Inverse the columns 
  m <- rbind(m, m[1,])
  st_polygon(list(m))
}

# Microplastics with detail as sf
microplastics_sf <- microplastics 
microplastics_sf$geom = st_sfc(sapply(1:nrow(microplastics_sf), 
                                      function(i){st_polygon_better(microplastics_sf[i,8:15])}, simplify = FALSE))
microplastics_sf <- microplastics_sf %>%
  st_sf(crs = 4326)  # Convert to sf object


# Microplastics total as sf
microplastics_total_sf <- microplastics_total
microplastics_total_sf$geom = st_sfc(sapply(1:nrow(microplastics_total_sf), 
                                            function(i){st_polygon_better(microplastics_total_sf[i,11:18])}, simplify = FALSE))
microplastics_total_sf <- microplastics_total_sf %>%
  st_sf(crs = 4326)  # Convert to sf object


# Remove rows for which the variables containing latitude are equal to 0 (missing data) for microplastics
microplastics_sf <- microplastics_sf %>% filter(Latitude_a != 0)
microplastics_total_sf <- microplastics_total_sf %>% filter(Latitude_a != 0)


# Viewing with map view
mapview(macroplastics_sf, zcol = "Volume_total", label = c("Site"), 
        popup = popupTable(macroplastics_sf, c(zcol = "Site", "Nom_zone", "Nom_evenement", "Date", "Annee", "Saison", "Volume_total", 
                                               "Longueur_lineaire", "Nb_participants", "Duree"))) +
  mapview(typologie_sites_sf, label = c("Localité"), 
          popup = popupTable(typologie_sites_sf, c(zcol = "Localité", "Commune", "Longueur", "Largeur", "Orientation")))

mapview(microplastics_total_sf, zcol = "Total_normalise", label = c("Site"))
