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

## Loading data ----
macrodechets_nb <- readRDS(paths$processed_macrodechets_nb)
microplastiques <- readRDS(paths$processed_microplastiques)
microplastiques_total <- readRDS(paths$processed_microplastiques_total)

# Preparing data ----

# Most common macrodechets all sites
macrodechets_all <- macrodechets_nb %>%
  filter(categorie_sub == "groupe") %>%
  group_by(categorie_specifique) %>%
  summarize(total_100m = sum(valeur_100m, na.rm = TRUE)) %>%
  mutate(total_100m = round(total_100m, 2)) %>%
  arrange(desc(total_100m)) %>%
  filter(total_100m > 0) %>%
  mutate(level = row_number()) %>%
  arrange(level) %>%
  mutate(print_name = paste(level, ":", str_replace_all(categorie_specifique, "_", " "))) %>%
  mutate(
    color = case_when(
      level <= 10 ~ "#e9770d",
      level <= 20 & level > 10 ~ "orange",
      TRUE ~ "#4da2db"
    )
  )

# Most common macrodechets selected sites
macrodechets_suivi <- macrodechets_nb %>%
  filter(site %in% sites_macrodechets) %>%
  filter(categorie_sub == "groupe") %>%
  group_by(categorie_specifique) %>%
  summarize(total_100m = sum(valeur_100m, na.rm = TRUE)) %>%
  mutate(total_100m = round(total_100m, 2)) %>%
  arrange(desc(total_100m)) %>%
  filter(total_100m > 0) %>%
  mutate(level = row_number()) %>%
  arrange(level) %>%
  mutate(print_name = paste(level, ":", str_replace_all(categorie_specifique, "_", " "))) %>%
  mutate(
    color = case_when(
      level <= 10 ~ "#e9770d",
      level <= 20 & level > 10 ~ "orange",
      TRUE ~ "#4da2db"
    )
  )

# Wordclouds ----

## Macrodechets all sites ----
dechets_wordcloud(
  dechet_data = macrodechets_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  save_name = "macrodechets_wordcloud_tous_sites_01.png"
)

dechets_wordcloud(
  dechet_data = macrodechets_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  save_name = "macrodechets_wordcloud_tous_sites_02.png"
)

dechets_wordcloud(
  dechet_data = macrodechets_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  save_name = "macrodechets_wordcloud_tous_sites_03.png",
  text_color = "#122c38"
)

dechets_wordcloud(
  dechet_data = macrodechets_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  save_name = "macrodechets_wordcloud_tous_sites_04.png",
  text_color = "random-light",
  background_color = "black"
)

dechets_wordcloud(
  dechet_data = macrodechets_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  save_name = "macrodechets_wordcloud_tous_sites_05.png",
  text_color = "random-light",
  background_color = "#1a1a1a"
)

## Macrodéchets suivi ----
dechets_wordcloud(
  dechet_data = macrodechets_suivi,
  # to_remove = c("Fragments_plastique_non-identifies"),
  offset = 100,
  graph_size = 0.5,
  save_name = "macrodechets_wordcloud_sites_suivis_01.png",
  text_color = "random-light",
  background_color = "#1a1a1a"
)

dechets_wordcloud(
  dechet_data = macrodechets_suivi,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 1,
  graph_size = 0.8,
  save_name = "macrodechets_wordcloud_sites_suivis_02.png",
  text_color = "random-light",
  background_color = "#1a1a1a"
)

dechets_wordcloud(
  dechet_data = macrodechets_suivi,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 1,
  graph_size = 0.8,
  save_name = "macrodechets_wordcloud_sites_suivis_03.png",
  text_color = "random-dark",
  background_color = "#ffffff"
)

# Treemap ----

total_objets <- sum(macrodechets_all$total_100m)

macrodechets_all2 <- macrodechets_all %>%
  dplyr::mutate(freq = total_100m / total_objets)


## All sites ----
treemap_style1 <- macrodechets_all2 %>%
  ggplot(., aes(area = freq, fill = level, label = print_name)) +
  treemapify::geom_treemap(layout = "srow") +
  treemapify::geom_treemap_text(
    place = "centre",
    layout = "srow",
    size = 18,
    reflow = TRUE,
    color = "white"
  ) +
  labs(title = "Proportion de déchets plastiques par groupe - tous sites confondus") +
  paletteer::scale_fill_paletteer_c("grDevices::Heat") +
  theme_pubr() +
  theme(legend.position = "none", plot.title = element_text(size = 30))

ggsave(
  plot = treemap_style1,
  width = 1000,
  height = 1200,
  scale = 4,
  filename = paste0(paths$output_mostcommon, "macrodechets_treemap_tous_sites_01.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

treemap_style2 <- macrodechets_all2 %>%
  ggplot(., aes(area = freq, fill = -level, label = print_name)) +
  treemapify::geom_treemap(layout = "srow") +
  treemapify::geom_treemap_text(place = "centre", layout = "srow", size = 18, reflow = TRUE, color = "#000000") +
  labs(title = "Proportion de déchets plastiques par groupe - tous sites confondus") +
  paletteer::scale_fill_paletteer_c("ggthemes::Orange Light") +
  # paletteer::scale_fill_paletteer_c("ggthemes::Classic Orange-White-Blue Light")   +
  theme_pubr() +
  theme(legend.position = "none", plot.title = element_text(size = 30))

ggsave(
  plot = treemap_style2,
  width = 1000,
  height = 1200,
  scale = 4,
  filename = paste0(paths$output_mostcommon, "macrodechets_treemap_tous_sites_02.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

treemap_style3 <- macrodechets_all2 %>%
  ggplot(., aes(area = freq, fill = -level, label = print_name)) +
  treemapify::geom_treemap(layout = "srow") +
  treemapify::geom_treemap_text(
    place = "centre",
    layout = "srow",
    size = 18,
    reflow = TRUE,
    color = "black"
  ) +
  labs(title = "Proportion de déchets plastiques par groupe - tous sites confondus") +
  paletteer::scale_fill_paletteer_c("grDevices::Heat") +
  theme_pubr() +
  theme(legend.position = "none", plot.title = element_text(size = 30))

ggsave(
  plot = treemap_style3,
  width = 1000,
  height = 1200,
  scale = 4,
  filename = paste0(paths$output_mostcommon, "macrodechets_treemap_tous_sites_03.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

## Selected sites ----
total_objets2 <- sum(macrodechets_suivi$total_100m)

macrodechets_suivi2 <- macrodechets_suivi %>%
  dplyr::mutate(freq = total_100m / total_objets2)

treemap_style1 <- macrodechets_suivi2 %>%
  ggplot(., aes(area = freq, fill = level, label = print_name)) +
  treemapify::geom_treemap(layout = "srow") +
  treemapify::geom_treemap_text(
    place = "centre",
    layout = "srow",
    size = 18,
    reflow = TRUE,
    color = "#ffffff"
  ) +
  labs(title = "Proportion de déchets plastiques par groupe - sites suivis") +
  paletteer::scale_fill_paletteer_c("grDevices::Heat") +
  theme_pubr() +
  theme(legend.position = "none", plot.title = element_text(size = 30))

ggsave(
  plot = treemap_style1,
  width = 1000,
  height = 1200,
  scale = 4,
  filename = paste0(paths$output_mostcommon, "macrodechets_treemap_sites_suivis_01.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

treemap_style2 <- macrodechets_suivi2 %>%
  ggplot(., aes(area = freq, fill = -level, label = print_name)) +
  treemapify::geom_treemap(layout = "srow") +
  treemapify::geom_treemap_text(place = "centre", layout = "srow", size = 18, reflow = TRUE, color = "#000000") +
  labs(title = "Proportion de déchets plastiques par groupe - sites suivis") +
  paletteer::scale_fill_paletteer_c("ggthemes::Orange Light") +
  # paletteer::scale_fill_paletteer_c("ggthemes::Classic Orange-White-Blue Light")   +
  theme_pubr() +
  theme(legend.position = "none", plot.title = element_text(size = 30))

ggsave(
  plot = treemap_style2,
  width = 1000,
  height = 1200,
  scale = 4,
  filename = paste0(paths$output_mostcommon, "macrodechets_treemap_sites_suivis_02.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

treemap_style3 <- macrodechets_suivi2 %>%
  ggplot(., aes(area = freq, fill = -level, label = print_name)) +
  treemapify::geom_treemap(layout = "srow") +
  treemapify::geom_treemap_text(
    place = "centre",
    layout = "srow",
    size = 18,
    reflow = TRUE,
    color = "black"
  ) +
  labs(title = "Proportion de déchets plastiques par groupe - sites suivis") +
  paletteer::scale_fill_paletteer_c("grDevices::Heat") +
  theme_pubr() +
  theme(legend.position = "none", plot.title = element_text(size = 30))

ggsave(
  plot = treemap_style3,
  width = 1000,
  height = 1200,
  scale = 4,
  filename = paste0(paths$output_mostcommon, "macrodechets_treemap_sites_suivis_03.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

# Barplot ----

g11 <- macrodechets_all %>%
  filter(total_100m >= 10) %>%
  ggplot(., aes(
    x = reorder(print_name, -total_100m),
    y = total_100m,
    fill = color
  )) +
  geom_bar(stat = "identity") +
  # scale_y_log10() +
  scale_fill_identity() +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "groupes", y = "abondance normalisée (sur 100m)")

ggsave(
  plot = g11,
  width = 2000,
  height = 1200,
  scale = 3,
  filename = paste0(paths$output_mostcommon, "macrodechets_barplot_tous_sites.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

# Barplot logscale
g12 <- macrodechets_all %>%
  filter(total_100m >= 10) %>%
  ggplot(., aes(
    x = reorder(print_name, -total_100m),
    y = total_100m,
    fill = color
  )) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  scale_fill_identity() +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "groupes", y = "abondance normalisée (sur 100m)")

ggsave(
  plot = g12,
  width = 2000,
  height = 1200,
  scale = 3,
  filename = paste0(paths$output_mostcommon, "macrodechets_barplot_tous_sites_echellelog.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

g21 <- macrodechets_suivi %>%
  filter(total_100m >= 10) %>%
  ggplot(., aes(
    x = reorder(print_name, -total_100m),
    y = total_100m,
    fill = color
  )) +
  geom_bar(stat = "identity") +
  # scale_y_log10() +
  scale_fill_identity() +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "groupes", y = "abondance normalisée (sur 100m)")

ggsave(
  plot = g21,
  width = 2000,
  height = 1200,
  scale = 3,
  filename = paste0(paths$output_mostcommon, "macrodechets_barplot_sites_suivis.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

g22 <- macrodechets_suivi %>%
  filter(total_100m >= 10) %>%
  ggplot(., aes(
    x = reorder(print_name, -total_100m),
    y = total_100m,
    fill = color
  )) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  scale_fill_identity() +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "groupes", y = "abondance normalisée (sur 100m)")

ggsave(
  plot = g22,
  width = 2000,
  height = 1200,
  scale = 3,
  filename = paste0(paths$output_mostcommon, "macrodechets_barplot_sites_suivis_echellelog.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)


# Highcharts test
# hchart(
#   macrodechets_all,
#   "item",
#   hcaes(
#     name = print_name,
#     y = total_100m / 100
#   ),
#   name = "mostcommon1",
#   center = list("50%", "75%"),
#   startAngle = -100,
#   endAngle = 00
# ) %>%
#   hc_title(text = "Macrodéchets les plus communs toutes plages confondues") %>%
#   hc_legend(labelFormat = '{name} <span style="opacity: 0.4">{y}</span>')

# Other ----
macrodechets_all_with_autres <- macrodechets_all %>%
  mutate(print_name = case_when(
    level > 30 ~ "Autres",
    TRUE ~ print_name
  )) %>%
  group_by(print_name) %>%
  summarize(
    total_100m = sum(total_100m),
    level = first(level)
  ) %>%
  ungroup()
