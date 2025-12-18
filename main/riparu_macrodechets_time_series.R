#' ---
#' title : "RIPARU - Macrodechets time series"
#' author : Aubin Woehrel
#' date : 2024-09-23
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - macrodechets TIME SERIES
#'
#' Description :
#' Time series of macrodechets counts
#'
#' =============================================================================

# Initialization ----

## Clean up of environment ----
rm(list = ls())

## Library imports -----

# Data import
library("openxlsx")
library("readxl")

# Data tidying
library("dplyr")
library("tidyr")
library("tibble")
library("stringr")

# Plotting
library("ggplot2")
library("ggpubr")
library("ggtext")

## Source paths ----
source("R/paths.R")
source("R/constants.R")

## Custom functions ----
source("R/fct_dechets_cat.R")
source("R/fct_dechets_timeseries.R")

## Loading data ----
macrodechets_nb <- readRDS(paths$processed_macrodechets_nb)

# Preparing data ----

# Checks
check_conditions <- macrodechets_nb %>%
  select(annee, date, site) %>%
  distinct() %>%
  arrange(date)

unique(macrodechets_nb$categorie_sub)

cat_types <- macrodechets_nb %>%
  select(categorie_sub, categorie_specifique) %>%
  distinct()

# Filter all dates of 2022 and after (in case of previous tests)
macrodechets_nb <- macrodechets_nb %>%
  filter(annee >= 2022) %>%
  mutate(mois = lubridate::month(date))

# REP ----
selection_rep <- c(
  "Articles_de_bricolage_et_de_jardin",
  "Articles_de_sport_et_de_loisirs",
  "Batiment",
  "Dechets_d'emballages_menagers",
  "Engins_de_peche",
  "Jouets",
  "Produits_du_tabac",
  "Textiles_sanitaires_a_usage_unique"
)

macrodechets_rep <- dechets_cat(
  categorie_sub_sel = "REP",
  sites_selected = sites_macrodechets,
  categorie_spe_sel = selection_rep,
  sum_by_cat = FALSE
)

palette_rep <- paletteer::paletteer_d("colorblindr::OkabeIto")

## Macrodechets REP count ----
dechets_timeseries(
  dechets_data = macrodechets_rep,
  subcat = "rep",
  save_name = "macrodechets_counts",
  point = TRUE,
  log_scale = FALSE,
  smooth = FALSE,
  palette_selected = palette_rep
)

dechets_timeseries(
  dechets_data = macrodechets_rep,
  subcat = "rep",
  save_name = "macrodechets_counts",
  point = TRUE,
  log_scale = TRUE,
  smooth = FALSE,
  palette_selected = palette_rep
)

dechets_timeseries(
  dechets_data = macrodechets_rep,
  subcat = "rep",
  save_name = "macrodechets_counts",
  point = TRUE,
  log_scale = FALSE,
  common_scale = TRUE,
  smooth = FALSE,
  palette_selected = palette_rep
)

macrodechets_rep %>%
  filter(site == "Alisu") %>%
  arrange(date) %>%
  pull(date) %>%
  unique()
## Macrodechets REP count with smooth ----

# Looping on a series of different spans for visual choices
for (spanwant in seq(from = 0.4, to = 0.9, by = 0.1)) {
  dechets_timeseries(
    dechets_data = macrodechets_rep,
    subcat = "rep",
    save_name = "macrodechets_counts",
    point = TRUE,
    log_scale = TRUE,
    smooth = TRUE,
    span = spanwant,
    palette_selected = palette_rep
  )
}

# Same but without points for better intuition of trends
for (spanwant in seq(from = 0.4, to = 0.9, by = 0.1)) {
  dechets_timeseries(
    dechets_data = macrodechets_rep,
    subcat = "rep",
    save_name = "macrodechets_counts_nopoints",
    point = FALSE,
    log_scale = TRUE,
    smooth = TRUE,
    span = spanwant,
    palette_selected = palette_rep
  )
}

# Secteurs ----
selection_secteurs <- c(
  "Alimentation",
  "Aquaculture",
  "Batiment_travaux_et_materiaux_de_construction",
  "Chasse_et_armement",
  "Cosmetiques_hygiene_et_soins_personnels",
  "Detergents_et_produits_d'entretiens",
  "Emballage_industriel_et_colis",
  "Jouets_et_loisir",
  "Plasturgie",
  "Peche",
  "Tabac",
  "Vaisselle_a_usage_unique"
)

palette_sect <- paletteer::paletteer_d("Polychrome::dark")

macrodechets_sect <- dechets_cat(
  categorie_sub_sel = "secteur",
  sites_selected = sites_macrodechets,
  categorie_spe_sel = selection_secteurs,
  sum_by_cat = FALSE
)

## Macrodechets secteurs count ----
dechets_timeseries(
  dechets_data = macrodechets_sect,
  subcat = "secteur",
  save_name = "macrodechets_counts",
  point = TRUE,
  log_scale = FALSE,
  smooth = FALSE,
  palette_selected = palette_sect
)

dechets_timeseries(
  dechets_data = macrodechets_sect,
  subcat = "secteur",
  save_name = "macrodechets_counts",
  point = TRUE,
  log_scale = TRUE,
  smooth = FALSE,
  palette_selected = palette_sect
)

## Macrodechets secteurs count with smooth ----

# Looping on a series of different spans for visual choices
for (spanwant in seq(from = 0.4, to = 0.9, by = 0.1)) {
  dechets_timeseries(
    dechets_data = macrodechets_sect,
    subcat = "secteur",
    save_name = "macrodechets_counts",
    point = TRUE,
    log_scale = TRUE,
    smooth = TRUE,
    span = spanwant,
    palette_selected = palette_sect
  )
}

# Same but without points for better intuition of trends
for (spanwant in seq(from = 0.4, to = 0.9, by = 0.1)) {
  dechets_timeseries(
    dechets_data = macrodechets_sect,
    subcat = "secteur",
    save_name = "macrodechets_counts_nopoints",
    point = FALSE,
    log_scale = TRUE,
    smooth = TRUE,
    span = spanwant,
    palette_selected = palette_sect
  )
}

# Standard (anciennement DCSMM) ----
selection_standard <- c(
  # "Alimentation", # N'existe plus
  "Bouchons-et-couvercles-de-bouteille",
  "Bouchons-et-couvercles-non-alimentaire",
  "Bouteilles-alimentaires-en-plastique-inferieures-a-0-5-l",
  "Bouteilles-alimentaires-en-plastique-superieures-a-0-5-l",
  "Bouteilles-et-contenants-produit-de-nettoyage",
  "Cartouches-et-bourre-de-chasse",
  "Contenants-alimentaire-en-polystyrene",
  "Emballages-alimentaires",
  "Emballages-alimentaires-autres",
  "Emballages-non-alimentaires-identifies",
  "Emballages-sucreries-et-chips",
  "Fragments-de-polystyrene-0-2-5-cm",
  "Fragments-de-polystyrene-2-5-50-cm",
  "Fragments-de-polystyrene-superieurs-a-50-cm",
  "Lingettes-jetables"
)

macrodechets_std <- dechets_cat(
  categorie_sub_sel = "standard",
  sites_selected = sites_macrodechets,
  categorie_spe_sel = selection_standard,
  sum_by_cat = FALSE
)

palette_std <- paletteer::paletteer_d("Polychrome::dark")

## Macrodechets standard count ----
dechets_timeseries(
  dechets_data = macrodechets_std,
  subcat = "standard",
  save_name = "macrodechets_counts",
  point = TRUE,
  log_scale = FALSE,
  smooth = FALSE,
  palette_selected = palette_std
)

dechets_timeseries(
  dechets_data = macrodechets_std,
  subcat = "standard",
  save_name = "macrodechets_counts",
  point = TRUE,
  log_scale = TRUE,
  smooth = FALSE,
  palette_selected = palette_std
)

## Macrodechets standard count with smooth ----

# Looping on a series of different spans for visual choices
for (spanwant in seq(from = 0.4, to = 0.9, by = 0.1)) {
  dechets_timeseries(
    dechets_data = macrodechets_std,
    subcat = "standard",
    save_name = "macrodechets_counts",
    point = TRUE,
    log_scale = TRUE,
    smooth = TRUE,
    span = spanwant,
    palette_selected = palette_std
  )
}

# Same but without points for better intuition of trends
for (spanwant in seq(from = 0.4, to = 0.9, by = 0.1)) {
  dechets_timeseries(
    dechets_data = macrodechets_std,
    subcat = "standard",
    save_name = "macrodechets_counts_nopoints",
    point = FALSE,
    log_scale = TRUE,
    smooth = TRUE,
    span = spanwant,
    palette_selected = palette_std
  )
}


# Groupe (anciennement ZDS) ----
selection_groupe <- c(
  "Batons_de_sucette",
  "Cotons-tiges",
  "Cotons-tiges_en_carton",
  "Medias_filtrants",
  "Pailles_en_plastique",
  "Sacs_plastique"
)

macrodechets_grp <- dechets_cat(
  categorie_sub_sel = "groupe",
  sites_selected = sites_macrodechets,
  categorie_spe_sel = selection_groupe,
  sum_by_cat = FALSE
)

palette_grp <- paletteer::paletteer_d("Polychrome::dark")

## Macrodechets secteurs count ----
dechets_timeseries(
  dechets_data = macrodechets_grp,
  subcat = "groupe",
  save_name = "macrodechets_counts",
  point = TRUE,
  log_scale = FALSE,
  smooth = FALSE,
  palette_selected = palette_grp
)

dechets_timeseries(
  dechets_data = macrodechets_grp,
  subcat = "groupe",
  save_name = "macrodechets_counts",
  point = TRUE,
  log_scale = TRUE,
  smooth = FALSE,
  palette_selected = palette_grp
)

## Macrodechets secteurs count with smooth ----

# Looping on a series of different spans for visual choices
for (spanwant in seq(from = 0.4, to = 0.9, by = 0.1)) {
  dechets_timeseries(
    dechets_data = macrodechets_grp,
    subcat = "groupe",
    save_name = "macrodechets_counts",
    point = TRUE,
    log_scale = TRUE,
    smooth = TRUE,
    span = spanwant,
    palette_selected = palette_grp
  )
}

# Same but without points for better intuition of trends
for (spanwant in seq(from = 0.4, to = 0.9, by = 0.1)) {
  dechets_timeseries(
    dechets_data = macrodechets_grp,
    subcat = "groupe",
    save_name = "macrodechets_counts_nopoints",
    point = FALSE,
    log_scale = TRUE,
    smooth = TRUE,
    span = spanwant,
    palette_selected = palette_grp
  )
}

# Others ? ---

macrodechets_mrq <- dechets_cat(
  categorie_sub_sel = "marque",
  sites_selected = sites_macrodechets,
  sum_by_cat = FALSE
)

macrodechets_gen <- dechets_cat(
  categorie_sub_sel = "generique",
  sites_selected = sites_macrodechets,
  sum_by_cat = FALSE
)

macrodechets_spe <- dechets_cat(
  categorie_sub_sel = "specifique",
  sites_selected = sites_macrodechets,
  sum_by_cat = FALSE
)
