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

# Clustering
library("factoextra")
library("ggdendro")

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
    acces_vehicule,
    ppuu_distribution,
    riviere_distance,
    ville_distance,
    ville_habitants,
    port_distance,
    step_distance
  ) %>%
  mutate(across(contains("distance"), \(x) {
    -x
  })) %>%
  mutate(across(where(is.numeric), ~ scales::rescale(., to = c(0, 1)))) %>%
  rename_with(
    ~ str_replace(.x, "distance", "proximité"),
    contains("distance")
  )

typologie_spider[typologie_spider == "oui"] <- "1"
typologie_spider[typologie_spider == "non"] <- "0"

typologie_spider <- typologie_spider %>%
  mutate(across(acces_vehicule:ppuu_distribution, \(x) as.numeric(x))) %>%
  pivot_longer(cols = -site) %>%
  mutate(categorie = case_when(
    # name %in% c("substrat_galets", "substrat_sable_fin") ~ "substrat", # not anymore substrates
    name %in% c(
      "riviere_proximité",
      "ville_proximité",
      "ville_habitants",
      "port_proximité",
      "step_proximité"
    ) ~ "Perturbations - extraplage",
    TRUE ~ "Perturbations - intraplage"
  ))

## Microplastiques total ----
microplastiques_spider <- microplastiques_total %>%
  group_by(site, saison) %>%
  summarize(meso_mean = mean(meso_normalise), micro_mean = mean(micro_normalise)) %>% # mean by season
  group_by(site) %>%
  summarize(mesoplastiques = sum(meso_mean), microplastiques = sum(micro_mean)) %>%
  mutate(across(where(is.numeric), \(x) log(x))) %>% # Log transfo for lesser visual effect
  mutate(across(where(is.numeric), ~ scales::rescale(., to = c(0.2, 1)))) %>% # Rescaling to values < 1
  pivot_longer(cols = -site) %>%
  mutate(categorie = "Microplastiques et Mésoplastiques") %>%
  ungroup()

## Macrodechets general ----
macrodechets_spider_general <- macrodechets_general %>%
  filter(site %in% sites_macrodechets) %>%
  filter(type == "poids_total_m2") %>%
  select(saison, date, annee, site, type, valeur) %>%
  group_by(saison, site, type) %>%
  summarize(valeur = mean(valeur)) %>%
  group_by(site, type) %>%
  summarize(valeur = mean(valeur)) %>%
  pivot_wider(names_from = type, values_from = valeur)

macrodechets_spider_general <- macrodechets_spider_general %>%
  ungroup() %>%
  mutate(across(c("poids_total_m2"), \(x) log(1 + x))) %>% # Log transfo for lesser visual effect
  mutate(across(c("poids_total_m2"), ~ scales::rescale(., to = c(0.2, 1)))) %>%
  # mutate(across(starts_with("global"), \(x) ifelse(x != 0, x / 100, x))) %>%
  pivot_longer(-site) %>%
  mutate(categorie = case_when(
    name %in% c("poids_total_m2") ~ "Macrodéchets - Total",
    TRUE ~ "Macrodéchets - composition globale"
  ))

macrodechets_spider_secteur <- macrodechets_nb %>%
  filter(site %in% sites_macrodechets) %>%
  filter(categorie_sub == "secteur") %>%
  filter(categorie_specifique %in% c(
    "Alimentation",
    "Cosmetiques_hygiene_et_soins_personnels",
    "Batiment_travaux_et_materiaux_de_construction",
    "Tabac",
    "Chasse_et_armement",
    "Peche"
  )) %>%
  select(saison, date, annee, site, categorie_specifique, valeur_m2) %>%
  group_by(saison, site, categorie_specifique) %>%
  summarize(valeur_m2 = mean(valeur_m2)) %>%
  group_by(site, categorie_specifique) %>%
  summarize(valeur_m2 = mean(valeur_m2)) %>%
  pivot_wider(names_from = categorie_specifique, values_from = valeur_m2)

macrodechets_spider_secteur <- macrodechets_spider_secteur %>%
  ungroup() %>%
  # mutate(across(-site, \(x) log(1 + x))) %>%
  # mutate(across(-site, ~ scales::rescale(., to = c(0.2, 1)))) %>%
  pivot_longer(-site) %>%
  mutate(categorie = "Macrodéchets - Secteurs majoritaires") %>%
  # transfo log(alpha * value + 1) with alpha increasing effect of log :
  mutate(value = scales::rescale(log(value * 200 + 1), to = c(0.2, 1)))

## All ----
all_spider <- rbind(typologie_spider, microplastiques_spider) %>%
  complete(site, name, fill = list(value = NA, categorie = "Microplastiques et Mésoplastiques")) %>%
  rbind(., macrodechets_spider_general) %>%
  arrange(site, categorie) %>%
  filter(site %in% all_sites) %>%
  # Attention si autre paramètre macrodechet
  complete(site, name, fill = list(value = NA, categorie = "Macrodéchets - Total")) %>%
  rbind(., macrodechets_spider_secteur) %>%
  arrange(site, categorie) %>%
  filter(site %in% all_sites) %>%
  # Attention si autre paramètre macrodechet
  complete(site, name, fill = list(value = NA, categorie = "Macrodéchets - Secteurs majoritaires"))


all_spider <- all_spider %>%
  mutate(name = case_when(
    name == "acces_vehicule" ~ "Accès aux véhicules",
    name == "ppuu_distribution" ~ "Distribution de PPUU",
    name == "mesoplastiques" ~ "Mésoplastiques",
    name == "microplastiques" ~ "Microplastiques",
    name == "poids_total_m2" ~ "Poids total macrodéchets",
    name == "port_proximité" ~ "Proximité port",
    name == "riviere_proximité" ~ "Proximité rivière",
    name == "ville_proximité" ~ "Proximité ville",
    name == "ville_habitants" ~ "Habitants ville",
    name == "step_proximité" ~ "Proximité STEP",
    name == "Cosmetiques_hygiene_et_soins_personnels" ~ "Cosmétique et Hygiène",
    name == "Batiment_travaux_et_materiaux_de_construction" ~ "Bâtiment, travaux et construction",
    name == "Chasse_et_armement" ~ "Chasse et armement",
    name == "Peche" ~ "Pêche",
    TRUE ~ name
  )) %>%
  arrange(site, categorie, name) %>%
  mutate(
    categorie = factor(categorie, levels = c(
      "Macrodéchets - Total",
      "Macrodéchets - Secteurs majoritaires",
      "Microplastiques et Mésoplastiques",
      "Perturbations - intraplage",
      "Perturbations - extraplage"
    )),
    name = factor(name, levels = unique(name))
  )

levels(all_spider$categorie)


# Clustering on all selected data ----

## Data preparation ----
clustering_data <- all_spider %>%
  select(-categorie) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  column_to_rownames("site") %>%
  mutate(across(everything(), \(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))) %>%
  .[, sapply(., function(v) var(v, na.rm = TRUE) != 0)]

## Kmeans ----
fviz_nbclust(clustering_data, FUNcluster = kmeans, k.max = 5) # 3 clusters best
cluster_1 <- kmeans(clustering_data, centers = 3)
g1 <- fviz_cluster(cluster_1, data = clustering_data) +
  theme_pubr() +
  labs(title = "Kmeans clustering") +
  paletteer::scale_fill_paletteer_d("ggthemes::Color_Blind") +
  paletteer::scale_color_paletteer_d("ggthemes::Color_Blind")

ggsave(
  plot = g1,
  width = 1000,
  height = 1000,
  scale = 3,
  filename = paste0(paths$output_spider, "kmeans.jpg"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

## Hclust ----
distance_matrix <- dist(clustering_data, method = "euclidean")
hc <- hclust(distance_matrix, method = "ward.D2")

### ggdendro test ----
ggdendrogram(hc) +
  geom_hline(yintercept = 1.8, color = "red")

### dendextend test ----
dendro <- as.dendrogram(hc) %>%
  dendextend::color_branches(k = 3)
plot(dendro)

### ggdendro extraction + ggplot ----
k <- 4 # clusters
dd <- ggdendro::dendro_data(hc, type = "rectangle")
seg <- dd$segments
lab <- dd$labels # x, y, label

# cluster labels
cl <- cutree(hc, k = k)
cl_df <- tibble(label = names(cl), cluster = as.integer(cl))
lab <- lab %>% left_join(cl_df, by = "label")

# palette
# pal <- paletteer::paletteer_d("nbapalettes::warriors_city") # 3 clusters
# pal <- paletteer::paletteer_d("NineteenEightyR::sunset1") # For 5 clusters
pal <- paletteer::paletteer_d("trekcolors::enara2") # For 4 clusters
names(pal) <- as.character(1:k)

# Increase angular spacing
spread <- 1.35
seg <- seg %>% mutate(across(c(x, xend), ~ .x * spread))
lab <- lab %>% mutate(x = x * spread)

# Map each segment to nearest leaf-cluster (coloring)
leaf_map <- lab %>%
  transmute(x_leaf = x, cluster) %>%
  distinct()

# Coloring of segments
seg_col <- seg %>%
  mutate(
    x_child = ifelse(yend < y, xend, x),
    y_child = pmin(y, yend)
  ) %>%
  rowwise() %>%
  mutate(cluster = leaf_map$cluster[which.min(abs(leaf_map$x_leaf - x_child))]) %>%
  ungroup() %>%
  mutate(col = pal[as.character(cluster)])

# Radial geometry for ring + labels + center padding
max_y <- max(seg$y, seg$yend, na.rm = TRUE)
leaf_y <- 0

outer_pad <- max_y * 0.08 # pushes the leaf ring outward
label_pad <- max_y * 0.14 # label radius beyond the ring
center_pad <- max_y * 0.3 # empty space at origin

ring_y <- leaf_y - outer_pad
text_y <- leaf_y - label_pad

# Padding to separate far left and right segments of original x scale
x_pad <- diff(range(lab$x)) * 0.04

# Tangential label rotation + left/right alignment
lab2 <- lab %>%
  mutate(
    angle = 90 - 360 * (x - min(x)) / (max(x) - min(x)),
    hjust = ifelse(angle < -90, 1, 0),
    angle = ifelse(angle < -90, angle + 180, angle)
  )

# Main plot
gdend <- ggplot() +
  geom_segment(
    data = seg_col,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 2.5,
    lineend = "round",
    colour = seg_col$col
  ) +
  geom_point(
    data = lab,
    aes(x = x, y = ring_y, colour = factor(cluster)),
    size = 4
  ) +
  geom_text(
    data = lab2,
    aes(
      x = x,
      y = text_y,
      label = label,
      angle = angle,
      hjust = hjust,
      colour = factor(cluster)
    ),
    size = 11,
    fontface = "bold"
  ) +
  scale_colour_manual(values = pal) +
  coord_polar(theta = "x", start = 0, clip = "off") +
  scale_x_continuous(limits = c(min(lab$x) - x_pad, max(lab$x) + x_pad)) +
  scale_y_reverse(limits = c(max_y, -center_pad)) + # <- adds space at origin
  theme_void(ink = "white") +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  plot = gdend,
  width = 1000,
  height = 1000,
  scale = 5,
  filename = paste0(paths$output_spider, "circular_dendogram.jpg"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

# Spider plots ----

## All categories ----
list_spiders <- NULL # List of graphs

for (site_plot in unique(all_spider$site)) {
  print(site_plot)

  spider_site <- all_spider %>%
    filter(site == site_plot) %>%
    mutate(angle = 90 - 360 * (row_number() - 0.5) / nrow(.)) %>%
    mutate(hjust = ifelse(angle < -90, 1, 0)) %>%
    mutate(angle = ifelse(angle < -90, angle + 180, angle))
  # mutate(name = str_replace_all(name, "_", " "))

  gspider <- spider_site %>%
    ggplot(., aes(x = name, y = value, fill = categorie)) +
    geom_col(position = "dodge2", color = "black") +
    geom_point(size = 4) +
    geom_text(
      aes(
        x = name,
        y = ifelse(value > 0.9, 0.3, ifelse(value > 0.2, value + 0.05, 0.3)),
        label = name,
        hjust = hjust,
        angle = angle
      ),
      color = "black",
      size = 9
    ) +
    coord_polar(start = 0) +
    theme_pubclean() +
    labs(title = site_plot, x = "") +
    ylim(0, 1.2) +
    # paletteer::scale_fill_paletteer_d("MetBrewer::Tiepolo") +
    # paletteer::scale_fill_paletteer_d("nationalparkcolors::GeneralGrant", direction = 1) +
    paletteer::scale_fill_paletteer_d("IslamicArt::cordoba", direction = 1) +
    theme(
      legend.position = "none",
      # Set default color and font family for the text
      # text = element_text(color = "gray12"),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      legend.title = element_text(size = 20, hjust = 0.05),
      legend.text = element_text(size = 15, hjust = 0.05),
      # Customize the text in the title, subtitle, and caption
      plot.title = element_text(face = "bold", size = 25, hjust = 0.05),
      plot.caption = element_text(size = 10, hjust = .5),

      # Make the background white and remove extra grid lines
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank()
    )

  ggsave(
    plot = gspider,
    width = 3000,
    height = 3000,
    scale = 2,
    filename = paste0(paths$output_spider, site_plot, ".jpg"),
    units = "px",
    dpi = "print",
    limitsize = FALSE
  )
  list_spiders <- c(list_spiders, gspider)
}

gg_all <- ggarrange(
  plotlist = list_spiders,
  nrow = 3,
  ncol = 3,
  common.legend = TRUE,
  legend = "right"
)

ggsave(
  plot = gg_all,
  width = 3200,
  height = 3000,
  scale = 5,
  filename = paste0(paths$output_spider, "all_sites.jpg"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)
