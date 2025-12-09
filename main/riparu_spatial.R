#' ---
#' title : "RIPARU - Spatial"
#' author : Aubin Woehrel
#' date : 2024-09-01
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - SPATIAL
#'
#' Description :
#' Spatial representation of data
#'
#' =============================================================================

# Initialization ----

## Clean up ----
rm(list = ls())

## Library imports ----

# Data tidying
library("dplyr")
library("tidyr")

# Plotting
library("ggplot2")
# library("readxl")

# Maps
library("mapview")
library("sf")
library("leafpop")

## Source paths and custom functions ----
source("R/paths.R")
source("R/constants.R")

## Data imports ----
microplastics <- readRDS(paths$processed_microplastiques)
microplastics_total <- readRDS(paths$processed_microplastiques_total)
typologie_sites <- readRDS(paths$processed_typologie_sites)

# Transforming datasets to sf format for map plotting

# macrodechets
macrodechets_sf <- macrodechets %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) # Convert to sf object

# Typologie sites
st_segment <- function(r) {
  st_linestring(t(matrix(unlist(r), 2, 2)))
}

typologie_sites$geom <- st_sfc(
  sapply(
    seq_len(nrow(typologie_sites)),
    function(i) {
      st_segment(typologie_sites[i, 1:4])
    },
    simplify = FALSE
  )
)

typologie_sites_sf <- typologie_sites %>%
  st_sf(crs = 4326) # Convert to sf object

# Microplastics


# Taking out Lozari due to the absence of cooordinates for this site
# TODO #1 : get coordinates of the site prospection in the future !
microplastics <- microplastics %>%
  filter(site != "Lozari")

microplastics_total <- microplastics_total %>%
  filter(site != "Lozari")

# Do the same for microplastics on the base of typologie site, but this time the geometry is a polygon defined by the
# points a, b, c, d at the coordinates Latitude_a, Longitude_a, Latitude_b, Longitude_b, that are at column 8 to 15

st_polygon_better <- function(r) {
  m <- matrix(unlist(r), 4, 2)
  # Change order to a, c, d, b
  m <- m[c(1, 3, 4, 2), ]
  m <- m[, c(2, 1)] # Inverse the columns
  m <- rbind(m, m[1, ])
  st_polygon(list(m))
}

# Microplastics with detail as sf
microplastics_sf <- microplastics
microplastics_sf$geom <- st_sfc(
  sapply(
    seq_len(nrow(microplastics_sf)),
    function(i) {
      st_polygon_better(microplastics_sf[i, 8:15])
    },
    simplify = FALSE
  )
)
microplastics_sf <- microplastics_sf %>%
  st_sf(crs = 4326) # Convert to sf object

# Microplastics total as sf
microplastics_total_sf <- microplastics_total
microplastics_total_sf$geom <- st_sfc(
  sapply(
    seq_len(nrow(microplastics_total_sf)),
    function(i) {
      st_polygon_better(microplastics_total_sf[i, 11:18])
    },
    simplify = FALSE
  )
)
microplastics_total_sf <- microplastics_total_sf %>%
  st_sf(crs = 4326) # Convert to sf object


# Viewing with map view ----

## Macrodechets and typologie sites
mapview(
  macrodechets_sf,
  zcol = "volume_total",
  label = c("site"),
  popup = popupTable(
    macrodechets_sf,
    c(
      zcol = "site", "nom_zone", "nom_evenement", "date", "annee", "saison", "volume_total",
      "longueur_lineaire", "nb_participants", "duree"
    )
  )
) +
  mapview(
    typologie_sites_sf,
    label = c("localite"),
    popup = popupTable(
      typologie_sites_sf,
      c(zcol = "localite", "commune", "longueur", "largeur", "orientation")
    )
  )

# Microplastics
mapview(microplastics_total_sf, zcol = "total_normalise", label = c("site"))
