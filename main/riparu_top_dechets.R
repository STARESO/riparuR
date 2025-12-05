#' ---
#' title : "RIPARU - Top déchets"
#' author : Aubin Woehrel
#' date : 2025-11-28
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - Top déchets
#'
#' Description :
#' Small script for representation of most important trash all times combined
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
library("treemapify")
# library("highcharter")

# Saving plots
library("webshot")
library("htmlwidgets")

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
microplastiques <- readRDS(paths$processed_microplastiques)
microplastiques_total <- readRDS(paths$processed_microplastiques_total)

# Preparing data ----
unique(macrodechets_nb$categorie_sub)

cat_types <- macrodechets_nb %>%
  select(categorie_sub, categorie_specifique) %>%
  distinct()

## Groupe ----
macrodechets_grp_all <- dechets_cat(categorie_sub_sel = "groupe", sites_selected = "all")
macrodechets_grp_suivi <- dechets_cat(categorie_sub_sel = "groupe", sites_selected = sites_macrodechets)

## Marque ----
macrodechets_mrq_all <- dechets_cat(categorie_sub_sel = "marque", sites_selected = "all")
macrodechets_mrq_suivi <- dechets_cat(categorie_sub_sel = "marque", sites_selected = sites_macrodechets)

## REP ----
macrodechets_rep_all <- dechets_cat(categorie_sub_sel = "REP", sites_selected = "all")
macrodechets_rep_suivi <- dechets_cat(categorie_sub_sel = "REP", sites_selected = sites_macrodechets)

## Secteur ----
macrodechets_sect_all <- dechets_cat(categorie_sub_sel = "secteur", sites_selected = "all")
macrodechets_sect_suivi <- dechets_cat(categorie_sub_sel = "secteur", sites_selected = sites_macrodechets)

## Standard ----
macrodechets_std_all <- dechets_cat(categorie_sub_sel = "standard", sites_selected = "all")
macrodechets_std_suivi <- dechets_cat(categorie_sub_sel = "standard", sites_selected = sites_macrodechets)

## Generique ----
macrodechets_gen_all <- dechets_cat(categorie_sub_sel = "generique", sites_selected = "all")
macrodechets_gen_suivi <- dechets_cat(categorie_sub_sel = "generique", sites_selected = sites_macrodechets)

## Specifique ----
macrodechets_spe_all <- dechets_cat(categorie_sub_sel = "specifique", sites_selected = "all")
macrodechets_spe_suivi <- dechets_cat(categorie_sub_sel = "specifique", sites_selected = sites_macrodechets)


# Treemaps ----

# Datasets and corresponding parameters
treemap_params <- list(
  list(data = macrodechets_grp_suivi, subcat = "groupe", suffix = "sites_suivis"),
  list(data = macrodechets_grp_all, subcat = "groupe", suffix = "tous_sites"),
  list(data = macrodechets_mrq_suivi, subcat = "marque", suffix = "sites_suivis"),
  list(data = macrodechets_mrq_all, subcat = "marque", suffix = "tous_sites"),
  list(data = macrodechets_rep_suivi, subcat = "rep", suffix = "sites_suivis"),
  list(data = macrodechets_rep_all, subcat = "rep", suffix = "tous_sites"),
  list(data = macrodechets_sect_suivi, subcat = "secteur", suffix = "sites_suivis"),
  list(data = macrodechets_sect_all, subcat = "secteur", suffix = "tous_sites"),
  list(data = macrodechets_std_suivi, subcat = "standard", suffix = "sites_suivis"),
  list(data = macrodechets_std_all, subcat = "standard", suffix = "tous_sites"),
  list(data = macrodechets_gen_suivi, subcat = "generique", suffix = "sites_suivis"),
  list(data = macrodechets_gen_all, subcat = "generique", suffix = "tous_sites")
)

# Looping treemap plotting custom function
for (treemap_param in treemap_params) {
  print(paste(treemap_param$subcat, "-", treemap_param$suffix))
  dechets_treemap(
    dechets_data = treemap_param$data,
    subcat = treemap_param$subcat,
    save_name = paste0("macrodechets_treemap_", treemap_param$suffix, ".png")
  )
}

# paletteer::scale_fill_paletteer_c("grDevices::Heat")
# paletteer::scale_fill_paletteer_c("ggthemes::Classic Orange-White-Blue Light")
# paletteer::scale_fill_paletteer_c("ggthemes::Orange Light")

# Barplots ----

# Test of custom function
dechets_barplot(
  dechets_data = macrodechets_grp_suivi,
  subcat = "tests",
  save_name = "macrodechets_barplottests",
  scale_log = TRUE
)

# Datasets and corresponding parameters
barplot_params <- list(
  list(data = macrodechets_grp_suivi, subcat = "groupe", suffix = "sites_suivis"),
  list(data = macrodechets_grp_all, subcat = "groupe", suffix = "tous_sites"),
  list(data = macrodechets_mrq_suivi, subcat = "marque", suffix = "sites_suivis"),
  list(data = macrodechets_mrq_all, subcat = "marque", suffix = "tous_sites"),
  list(data = macrodechets_rep_suivi, subcat = "rep", suffix = "sites_suivis"),
  list(data = macrodechets_rep_all, subcat = "rep", suffix = "tous_sites"),
  list(data = macrodechets_sect_suivi, subcat = "secteur", suffix = "sites_suivis"),
  list(data = macrodechets_sect_all, subcat = "secteur", suffix = "tous_sites"),
  list(data = macrodechets_std_suivi, subcat = "standard", suffix = "sites_suivis"),
  list(data = macrodechets_std_all, subcat = "standard", suffix = "tous_sites"),
  list(data = macrodechets_gen_suivi, subcat = "generique", suffix = "sites_suivis"),
  list(data = macrodechets_gen_all, subcat = "generique", suffix = "tous_sites")
)

# Looping barplot plotting custom function
for (barplot_param in barplot_params) {
  print(paste(barplot_param$subcat, "-", barplot_param$suffix))

  for (scale_log in c(FALSE, TRUE)) {
    dechets_barplot(
      dechets_data = barplot_param$data,
      subcat = barplot_param$subcat,
      save_name = paste0("macrodechets_barplots_", barplot_param$suffix),
      scale_log = scale_log
    )
  }
}

# Wordclouds ----

## Groupes ----
dechets_wordcloud(
  dechets_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_grp_suivi,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)

## Marques ----
dechets_wordcloud(
  dechets_data = macrodechets_mrq_all,
  offset = 1,
  graph_size = 0.9,
  subcat = "marque",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_mrq_suivi,
  offset = 1,
  graph_size = 0.9,
  subcat = "marque",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)

## REP ----
dechets_wordcloud(
  dechets_data = macrodechets_rep_all,
  to_remove = c("Vide"),
  offset = 10,
  graph_size = 0.5,
  subcat = "rep",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_rep_suivi,
  to_remove = c("Vide"),
  offset = 10,
  graph_size = 0.5,
  subcat = "rep",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)

## Secteurs ----
dechets_wordcloud(
  dechets_data = macrodechets_sect_all,
  offset = 1,
  graph_size = 0.9,
  subcat = "secteur",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_sect_suivi,
  offset = 1,
  graph_size = 0.9,
  subcat = "secteur",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)

## Standard ----
dechets_wordcloud(
  dechets_data = macrodechets_std_all,
  offset = 100,
  graph_size = 0.4,
  subcat = "standard",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_std_suivi,
  offset = 100,
  graph_size = 0.4,
  subcat = "standard",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)

## Generique ----
dechets_wordcloud(
  dechets_data = macrodechets_gen_all,
  offset = 1,
  graph_size = 0.4,
  subcat = "generique",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_gen_suivi,
  offset = 1,
  graph_size = 0.4,
  subcat = "generique",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)





# Wordcloud style tests ----
## Groupe ----
### Macrodechets groupe all sites ----
dechets_wordcloud(
  dechet_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_groupe_tous_sites_01.png"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_groupe_tous_sites_02.png"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_groupe_tous_sites_03.png",
  text_color = "#122c38"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_groupe_tous_sites_04.png",
  text_color = "random-light",
  background_color = "black"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_groupe_tous_sites_05.png",
  text_color = "random-light",
  background_color = "#1a1a1a"
)

### Macrodéchets groupe suivi ----
dechets_wordcloud(
  dechet_data = macrodechets_grp_suivi,
  # to_remove = c("Fragments_plastique_non-identifies"),
  offset = 100,
  graph_size = 0.5,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_sites_suivis_01.png",
  text_color = "random-light",
  background_color = "#1a1a1a"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_suivi,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 1,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_sites_suivis_02.png",
  text_color = "random-light",
  background_color = "#1a1a1a"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_suivi,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 1,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_sites_suivis_03.png",
  text_color = "random-dark",
  background_color = "#ffffff"
)
