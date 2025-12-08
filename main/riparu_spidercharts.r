#' ---
#' title : "RIPARU - spidercharts"
#' author : Aubin Woehrel
#' date : 2025-12-08
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - SPIDERCHARTS
#'
#' Description :
#' Script for representing data with spidercharts
#'
#' =============================================================================

# Initialization ----

## Clean up of environment ----
rm(list = ls())

## Library imports -----

# Data tidying
library("dplyr")
library("tidyr")
library("tibble")
library("stringr")

# Plotting
library("ggplot2")
library("ggpubr")

## Source paths & constants----
source("R/paths.R")
source("R/constants.R")

## Source custom functions ----
source("R/fct_dechets_wordcloud.R")
source("R/fct_dechets_cat.R")
source("R/fct_dechets_treemap.R")
source("R/fct_dechets_barplot.R")

## Loading data ----
macrodechets_nb <- readRDS(paths$processed_macrodechets_nb)
macrodechets_general <- readRDS(paths$processed_macrodechets_general)
microplastiques <- readRDS(paths$processed_microplastiques)
microplastiques_total <- readRDS(paths$processed_microplastiques_total)
typologie_sites <- readRDS(paths$processed_typologie_sites)

# Preparing data ----

# Verifying if all studied sites are in typologie
all_sites <- unique(c(sites_macrodechets, sites_microplastiques))
all_sites %in% unique(typologie_sites$site)

## Preparing typologie_sites for spiderplots
typologie_spider <- typologie_sites %>%
  select(
    site,
    substrat_galets,
    substrat_sable_fin,
    acces_vehicule,
    ppuu_distribution,
    riviere_distance,
    ville_distance,
    ville_habitants,
    port_distance,
    step_distance
  ) %>%
  mutate(across(where(is.numeric), ~ scales::rescale(., to = c(0, 1))))

typologie_spider[typologie_spider == "oui"] <- "1"
typologie_spider[typologie_spider == "non"] <- "0"

typologie_spider <- typologie_spider %>%
  mutate(across(substrat_galets:ppuu_distribution, \(x) as.numeric(x))) %>%
  pivot_longer(cols = -site)


# Spider plot


typologie_spider %>%
  filter(site == "Alisu") %>%
  ggplot(., aes(x = name, y = value, fill = value)) +
  geom_col(position = "dodge2") +
  coord_polar() +
  theme_pubclean()


list_spiders <- NULL
for (site_plot in unique(typologie_spider$site)) {
  print(site_plot)
  gspider <- typologie_spider %>%
    filter(site == site_plot) %>%
    ggplot(., aes(x = name, y = value, fill = value)) +
    geom_col(position = "dodge2") +
    coord_polar() +
    theme_pubclean() +
    labs(subtitle = site_plot, x = element_blank()) +
    theme(legend.position = "none")
  list_spiders <- c(list_spiders, gspider)
}

ggarrange(plotlist = list_spiders, nrow = 4, ncol = 3)
