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

## Typologie sites ----

# Verifying if all studied sites are in typologie
all_sites <- unique(c(sites_macrodechets, sites_microplastiques))
all_sites %in% unique(typologie_sites$site)

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
  pivot_longer(cols = -site) %>%
  mutate(categorie = case_when(
    name %in% c("substrat_galets", "substrat_sable_fin") ~ "substrat",
    name %in% c(
      "riviere_distance",
      "ville_distance",
      "ville_habitants",
      "port_distance",
      "step_distance"
    ) ~ "perturbations - extraplage",
    TRUE ~ "perturbations - intraplage"
  )) %>%
  arrange(site, categorie, name) %>%
  mutate(
    categorie = as.factor(categorie),
    name = factor(name, levels = unique(name))
  )


## Microplastiques total ----
microplastiques_spider <- microplastiques_total %>%
  group_by(site, saison) %>%
  summarize(meso_mean = mean(meso_normalise), micro_mean = mean(micro_normalise)) %>% # mean by season
  group_by(site) %>%
  summarize(mesoplastiques = sum(meso_mean), microplastiques = sum(micro_mean)) %>%
  mutate(across(where(is.numeric), \(x) log(x))) %>%
  mutate(across(where(is.numeric), ~ scales::rescale(., to = c(0.2, 1)))) %>%
  pivot_longer(cols = -site) %>%
  mutate(categorie = "micromesoplastiques") %>%
  ungroup()

## Macrodechets general ----
macrodechets_spider <- macrodechets_general %>%
  filter(site %in% sites_macrodechets) %>%
  filter(type %in% c(
    "volume_total_100m",
    "poids_total_100m"
    # "global_pourcentage_plastique",
    # "global_pourcentage_caoutchouc",
    # "global_pourcentage_bois",
    # "global_pourcentage_textile",
    # "global_pourcentage_papier",
    # "global_pourcentage_metal",
    # "global_pourcentage_verre",
    # "global_pourcentage_autre"
  )) %>%
  select(saison, date, annee, site, type, valeur) %>%
  group_by(saison, site, type) %>%
  summarize(valeur = mean(valeur)) %>%
  group_by(site, type) %>%
  summarize(valeur = mean(valeur)) %>%
  pivot_wider(names_from = type, values_from = valeur)

macrodechets_spider <- macrodechets_spider %>%
  ungroup() %>%
  mutate(across(c("poids_total_100m", "volume_total_100m"), \(x) log(1 + x))) %>%
  mutate(across(c("poids_total_100m", "volume_total_100m"), ~ scales::rescale(., to = c(0.2, 1)))) %>%
  # mutate(across(starts_with("global"), \(x) ifelse(x != 0, x / 100, x))) %>%
  pivot_longer(-site) %>%
  mutate(categorie = case_when(
    name %in% c("volume_total_100m", "poids_total_100m") ~ "macrodechets - total",
    TRUE ~ "macrodechets - composition globale"
  ))

## All ----
all_spider <- rbind(typologie_spider, microplastiques_spider) %>%
  complete(site, name, fill = list(value = NA, categorie = "micromesoplastiques")) %>%
  rbind(., macrodechets_spider) %>%
  arrange(site, categorie) %>%
  filter(site %in% all_sites) %>%
  complete(site, name, fill = list(value = NA, categorie = "macrodechets - total")) # Attention si autre paramètre macrodechet


# Spider plots ----


## All categories ----

list_spiders <- NULL
for (site_plot in unique(all_spider$site)) {
  print(site_plot)

  gspider <- all_spider %>%
    filter(site == site_plot) %>%
    ggplot(., aes(x = name, y = value, fill = categorie)) +
    geom_col(position = "dodge2") +
    geom_point() +
    coord_polar() +
    theme_pubclean() +
    labs(title = site_plot, x = "") +
    theme(
      legend.position = "none",
      # Set default color and font family for the text
      text = element_text(color = "gray12"),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      # Customize the text in the title, subtitle, and caption
      plot.title = element_text(face = "bold", size = 25, hjust = 0.05),
      plot.subtitle = element_text(size = 14, hjust = 0.05),
      plot.caption = element_text(size = 10, hjust = .5),

      # Make the background white and remove extra grid lines
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank()
    )
  list_spiders <- c(list_spiders, gspider)
}

gg_all <- ggarrange(
  plotlist = list_spiders,
  nrow = 3,
  ncol = 3,
  common.legend = TRUE
)

ggsave(
  plot = gg_all,
  width = 3000,
  height = 3000,
  scale = 2.75,
  filename = paste0(paths$output_spider, "all_sites.jpg"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)


## Spider ----
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
    labs(subtitle = site_plot, x = "") +
    theme(legend.position = "none")
  list_spiders <- c(list_spiders, gspider)
}

ggarrange(plotlist = list_spiders, nrow = 4, ncol = 3)



## Microplastiques ----
list_spiders <- NULL
for (site_plot in unique(microplastiques_spider$site)) {
  print(site_plot)
  gspider <- microplastiques_spider %>%
    filter(site == site_plot) %>%
    ggplot(., aes(x = name, y = value, fill = value)) +
    geom_col(position = "dodge2") +
    coord_polar() +
    theme_pubclean() +
    labs(subtitle = site_plot, x = "") +
    theme(legend.position = "none")
  list_spiders <- c(list_spiders, gspider)
}

ggarrange(plotlist = list_spiders, nrow = 4, ncol = 3)
