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
library("treemap")
# library("highcharter")

## Source paths & constants----
source("R/paths.R")
source("R/constants.R")

## Source custom functions ----
source("R/fct_dechets_wordcloud.R")

## Loading data ----
macrodechets_nb <- readRDS(paths$processed_macrodechets_nb)
microplastiques <- readRDS(paths$processed_microplastiques)
microplastiques_total <- readRDS(paths$processed_microplastiques_total)
typologie_sites <- readRDS(paths$processed_typologie_sites)
# macrodechets_general <- readRDS(paths$processed_macrodechets_general)

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
  graph_size = 0.5
)

dechets_wordcloud(
  dechet_data = macrodechets_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.5
)

dechets_wordcloud(
  dechet_data = macrodechets_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.5,
  text_color = "#122c38"
)

dechets_wordcloud(
  dechet_data = macrodechets_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.5,
  text_color = "random-light",
  background_color = "black"
)

dechets_wordcloud(
  dechet_data = macrodechets_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.5,
  text_color = "random-light",
  background_color = "#1a1a1a"
)

## Macrodéchets suivi ----
dechets_wordcloud(
  dechet_data = macrodechets_suivi,
  # to_remove = c("Fragments_plastique_non-identifies"),
  offset = 100,
  graph_size = 0.2,
  text_color = "random-light",
  background_color = "#1a1a1a"
)

dechets_wordcloud(
  dechet_data = macrodechets_suivi,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 1,
  graph_size = 0.4,
  text_color = "random-light",
  background_color = "#1a1a1a"
)


# Treemap ----
macrodechets_all_with_autres <- macrodechets_all %>%
  mutate(print_name = case_when(
    level > 30 ~ "Autres",
    TRUE ~ print_name
  )) %>%
  group_by(print_name) %>%
  summarize(
    total_100m = sum(total_100m),
    total_log = sum(total_log),
    level = first(level)
  ) %>%
  ungroup()

macrodechets_all %>%
  filter(total_100m >= 10) %>%
  treemap(., index = "print_name", vSize = "total_100m", type = "index")

# Barplot
g12 <- macrodechets_all %>%
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
  plot = g12,
  width = 2000,
  height = 1200,
  scale = 3,
  filename = paste0(paths$output_mostcommon, "macrodechets_allsites.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

# Barplot logscale
g13 <- macrodechets_all %>%
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
  plot = g13,
  width = 2000,
  height = 1200,
  scale = 3,
  filename = paste0(paths$output_mostcommon, "macrodechets_allsites_logscale.png"),
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


g2 <- macrodechets_suivi %>%
  filter(total_100m >= 10) %>%
  treemap(., index = "categorie_specifique", vSize = "total_100m")

g22 <- macrodechets_suivi %>%
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
  plot = g22,
  width = 2000,
  height = 1200,
  scale = 3,
  filename = paste0(paths$output_mostcommon, "macrodechets_selectedsites.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)
