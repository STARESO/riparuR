#' ---
#' title : "RIPARU - Macrodéchets PCA"
#' author : Aubin Woehrel
#' date : 2024-09-26
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - MACRODECHETS PCA
#'
#' Description :
#' This script contains the code for the PCA analysis of the macrodéchets data.
#'
#' =============================================================================


# Initialization ----

## Clean up ----
rm(list = ls())

## Library imports ----

# Data manipulation
library("dplyr")
library("tidyr")

# PCA
library("factoextra")
library("echarts4r")

## Source paths and useful functions ----
source("R/paths.R")
source("R/fct_pca_riparu.R")

## Data imports ----
macrodechets_nb <- readRDS(paths$processed_macrodechets_nb)

## Filtering unusable data ----

# Removing sites with no normalized value over 100m
sites_to_remove <- macrodechets_nb %>%
  filter(is.na(valeur_100m)) %>%
  pull(site) %>%
  unique()

macrodechets_nb <- macrodechets_nb %>%
  filter(!site %in% c(sites_to_remove))

# PCA filière REP ----

## Data tidying and preparation ----

macrodechets_rep <- macrodechets_selection(
  macrodechets_counts = macrodechets_nb,
  cat_choice = "REP",
  categorie_removal = c("Vide")
  # "Véhicules hors d’usage (vhu)"
)
macrodechets_rep_hellinger <- vegan::decostand(macrodechets_rep, method = "hellinger")
macrodechets_rep_habillage <- macrodechets_habillage(macrodechets_nb, "REP")

## PCA Computing ----
rep_pca <- prcomp(macrodechets_rep_hellinger, center = TRUE, scale. = FALSE)
summary(rep_pca)
fviz_eig(rep_pca)

paran::paran(macrodechets_rep_hellinger,
  iterations = 10000,
  quietly = FALSE,
  status = TRUE,
  all = TRUE,
  cfa = FALSE,
  graph = TRUE,
  color = FALSE,
  col = c("black", "red", "blue"),
  lty = c(1, 2, 3),
  lwd = 1,
  legend = TRUE,
  file = "",
  width = 640,
  height = 640,
  grdevice = "png",
  seed = 0,
  mat = NA,
  n = NA
)

# Hopkins statistics on the dataset to determine if it is suitable for clustering
hopK <- hopkins::hopkins(macrodechets_rep_hellinger)
hopK # Close to 1
hopkins::hopkins.pval(hopK, dim(macrodechets_rep_hellinger)[1]) # significant pvalue

fviz_nbclust(rep_pca$x[, c(1:2)], FUNcluster = kmeans, k.max = 10) # 2 clusters
fviz_nbclust(rep_pca$x[, c(1:2)], FUNcluster = kmeans, k.max = 10, method = "wss") # 2 clusters


## PCA plotting ----
nice_pca_plot(rep_pca, axes = c(1, 2), nb_axes = 2, cluster_number = 2, contrib = 7) %>%
  ggsave(
    filename = "figures/pca/rep/pca_filiere_rep_axes12.png",
    width = 2000,
    height = 1080,
    scale = 4,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

# nice_pca_plot(rep_pca, axes = c(1, 3), nb_axes = 3, cluster_number = 2, contrib = 7) %>%
#   ggsave(
#     filename = "figures/pca/rep/pca_filiere_rep_axes13.png",
#     width = 2000,
#     height = 1080,
#     scale = 4,
#     units = "px",
#     limitsize = FALSE,
#     dpi = 300,
#     bg = "white"
#   )

# nice_pca_plot(rep_pca, axes = c(2, 3), nb_axes = 3, cluster_number = 2, contrib = 7) %>%
#   ggsave(
#     filename = "figures/pca/rep/pca_filiere_rep_axes23.png",
#     width = 2000,
#     height = 1080,
#     scale = 4,
#     units = "px",
#     limitsize = FALSE,
#     dpi = 300,
#     bg = "white"
#   )


## PCA effect groups ----
effet_group_biplots(rep_pca, macrodechets_rep_habillage, contrib = 7) %>%
  ggsave(
    filename = "figures/pca/rep/pca_filiere_rep_groups.png",
    width = 4000,
    height = 1000,
    scale = 3,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

effet_group_biplots(rep_pca, macrodechets_rep_habillage, addEllipses = FALSE, contrib = 7) %>%
  ggsave(
    filename = "figures/pca/rep/pca_filiere_rep_groups_no_ellipses.png",
    width = 4000,
    height = 1000,
    scale = 3,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )


# 3D representation :
rep_3D <- rep_pca$x[, c(1:3)] %>%
  as.data.frame() %>%
  tibble::rownames_to_column("id") %>%
  left_join(macrodechets_rep_habillage %>% tibble::rownames_to_column("id"), by = "id")

# Prepare the PCA vector data for start and end points
pca_vector_points <- rep_pca$rotation[, 1:3] %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable") %>%
  mutate(
    start_PC1 = 0, # All vectors start from the origin for PC1, PC2, PC3
    start_PC2 = 0,
    start_PC3 = 0,
    end_PC1 = PC1, # Use PCA loadings for end coordinates
    end_PC2 = PC2,
    end_PC3 = PC3
  )

# Create a base 3D scatter plot for PCA points
p <- rep_3D %>%
  group_by(site) %>%
  e_charts(PC1) %>%
  e_scatter_3d(PC2, PC3, color = id) %>%
  e_labels(position = "top") %>%
  e_theme("vintage") %>%
  e_tooltip(trigger = "item")


# PCA Secteurs ----
macrodechets_secteurs <- macrodechets_selection(macrodechets_nb, "secteur", categorie_removal = c("Indéterminé"))
macrodechets_secteurs_hellinger <- vegan::decostand(macrodechets_secteurs, method = "hellinger")
macrodechets_secteurs_habillage <- macrodechets_habillage(macrodechets_nb, "secteur")

## Data tyding and preparation ----
secteurs_pca <- prcomp(macrodechets_secteurs_hellinger, center = TRUE, scale. = FALSE)
summary(secteurs_pca)
fviz_eig(secteurs_pca)

# paran
paran::paran(macrodechets_secteurs_hellinger,
  iterations = 10000,
  quietly = FALSE,
  status = TRUE,
  all = TRUE,
  cfa = FALSE,
  graph = TRUE,
  color = FALSE,
  col = c("black", "red", "blue"),
  lty = c(1, 2, 3),
  lwd = 1,
  legend = TRUE,
  file = "",
  width = 640,
  height = 640,
  grdevice = "png",
  seed = 0,
  mat = NA,
  n = NA
)
# Hopkins statistics
hopK <- hopkins::hopkins(macrodechets_secteurs_hellinger)
hopK # Close to 1
hopkins::hopkins.pval(hopK, dim(macrodechets_secteurs_hellinger)[1]) # significant pvalue

fviz_nbclust(secteurs_pca$x[, c(1:2)], FUNcluster = kmeans, k.max = 10) # 3 clusters
fviz_nbclust(secteurs_pca$x[, c(1:2)], FUNcluster = kmeans, k.max = 10, method = "wss") # 3 clusters


## PCA plotting ----
nice_pca_plot(secteurs_pca, axes = c(1, 2), nb_axes = 2, cluster_number = 3) %>%
  ggsave(
    filename = "figures/pca/secteurs/pca_secteurs_axes12.png",
    width = 2000,
    height = 1080,
    scale = 4,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

effet_group_biplots(secteurs_pca, macrodechets_secteurs_habillage, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/secteurs/pca_secteurs_groups.png",
    width = 4000,
    height = 1000,
    scale = 3,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

effet_group_biplots(secteurs_pca, macrodechets_secteurs_habillage, addEllipses = FALSE, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/secteurs/pca_secteurs_groups_no_ellipses.png",
    width = 4000,
    height = 1000,
    scale = 3,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )


# PCA DCSMM ----
macrodechets_DCSMM <- macrodechets_selection(macrodechets_nb, "DCSMM")
macrodechets_DCSMM_hellinger <- vegan::decostand(macrodechets_DCSMM, method = "hellinger")
macrodechets_DCSMM_habillage <- macrodechets_habillage(macrodechets_nb, "DCSMM")

## Data tyding and preparation ----
DCSMM_pca <- prcomp(macrodechets_DCSMM_hellinger, center = TRUE, scale. = FALSE)
summary(DCSMM_pca)
fviz_eig(DCSMM_pca)

# paran
paran::paran(macrodechets_DCSMM_hellinger,
  iterations = 2000,
  quietly = FALSE,
  status = TRUE,
  all = TRUE,
  cfa = FALSE,
  graph = TRUE,
  color = FALSE,
  col = c("black", "red", "blue"),
  lty = c(1, 2, 3),
  lwd = 1,
  legend = TRUE,
  file = "",
  width = 640,
  height = 640,
  grdevice = "png",
  seed = 0,
  mat = NA,
  n = NA
)

# Hopkins statistics
hopK <- hopkins::hopkins(macrodechets_DCSMM_hellinger)
hopK # Close to 1
hopkins::hopkins.pval(hopK, dim(macrodechets_DCSMM_hellinger)[1]) # significant pvalue

fviz_nbclust(DCSMM_pca$x[, c(1:3)], FUNcluster = kmeans, k.max = 10) # 3 clusters
fviz_nbclust(DCSMM_pca$x[, c(1:3)], FUNcluster = kmeans, k.max = 10, method = "wss") # 4 clusters


## PCA plotting ----
nice_pca_plot(DCSMM_pca, axes = c(1, 2), nb_axes = 3, cluster_number = 3, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/DCSMM/pca_DCSMM_axes12.png",
    width = 2000,
    height = 1080,
    scale = 4,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

nice_pca_plot(DCSMM_pca, axes = c(1, 3), nb_axes = 3, cluster_number = 3, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/DCSMM/pca_DCSMM_axes13.png",
    width = 2000,
    height = 1080,
    scale = 4,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

nice_pca_plot(DCSMM_pca, axes = c(2, 3), nb_axes = 3, cluster_number = 3, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/DCSMM/pca_DCSMM_axes23.png",
    width = 2000,
    height = 1080,
    scale = 4,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )


effet_group_biplots(DCSMM_pca, macrodechets_DCSMM_habillage, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/DCSMM/pca_DCSMM_groups.png",
    width = 4000,
    height = 1000,
    scale = 3,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

effet_group_biplots(DCSMM_pca, macrodechets_DCSMM_habillage, addEllipses = FALSE, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/DCSMM/pca_DCSMM_groups_no_ellipses.png",
    width = 4000,
    height = 1000,
    scale = 3,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )


# PCA groupe ----
macrodechets_groupe <- macrodechets_selection(macrodechets_nb, "groupe")
macrodechets_groupe_hellinger <- vegan::decostand(macrodechets_groupe, method = "hellinger")
macrodechets_groupe_habillage <- macrodechets_habillage(macrodechets_nb, "groupe")

## Data tyding and preparation ----
groupe_pca <- prcomp(macrodechets_groupe_hellinger, center = TRUE, scale. = FALSE)
summary(groupe_pca)
fviz_eig(groupe_pca)

# paran
paran::paran(macrodechets_groupe_hellinger,
  iterations = 5000,
  quietly = FALSE,
  status = TRUE,
  all = TRUE,
  cfa = FALSE,
  graph = TRUE,
  color = FALSE,
  col = c("black", "red", "blue"),
  lty = c(1, 2, 3),
  lwd = 1,
  legend = TRUE,
  file = "",
  width = 640,
  height = 640,
  grdevice = "png",
  seed = 0,
  mat = NA,
  n = NA
)

# Hopkins statistics
hopK <- hopkins::hopkins(macrodechets_groupe_hellinger)
hopK # Close to 1
hopkins::hopkins.pval(hopK, dim(macrodechets_groupe_hellinger)[1]) # significant pvalue

fviz_nbclust(groupe_pca$x[, c(1:3)], FUNcluster = kmeans, k.max = 10) # 3 clusters
fviz_nbclust(groupe_pca$x[, c(1:3)], FUNcluster = kmeans, k.max = 10, method = "wss") # 3 clusters


## PCA plotting ----
nice_pca_plot(groupe_pca, axes = c(1, 2), nb_axes = 3, cluster_number = 3, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/groupe/pca_groupe_axes12.png",
    width = 2000,
    height = 1080,
    scale = 4,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

nice_pca_plot(groupe_pca, axes = c(1, 3), nb_axes = 3, cluster_number = 3, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/groupe/pca_groupe_axes13.png",
    width = 2000,
    height = 1080,
    scale = 4,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

nice_pca_plot(groupe_pca, axes = c(2, 3), nb_axes = 3, cluster_number = 3, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/groupe/pca_groupe_axes23.png",
    width = 2000,
    height = 1080,
    scale = 4,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )


effet_group_biplots(groupe_pca, macrodechets_groupe_habillage, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/groupe/pca_groupe_groups.png",
    width = 4000,
    height = 1000,
    scale = 3,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

effet_group_biplots(groupe_pca, macrodechets_groupe_habillage, addEllipses = FALSE, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/groupe/pca_groupe_groups_no_ellipses.png",
    width = 4000,
    height = 1000,
    scale = 3,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )


# PCA marque ----
macrodechets_marque <- macrodechets_selection(macrodechets_nb, "marque")
macrodechets_marque_hellinger <- vegan::decostand(macrodechets_marque, method = "hellinger")
macrodechets_marque_habillage <- macrodechets_habillage(macrodechets_nb, "marque")

## Data tyding and preparation ----
marque_pca <- prcomp(macrodechets_marque_hellinger, center = TRUE, scale. = FALSE)
summary(marque_pca)
fviz_eig(marque_pca)

# paran
paran::paran(macrodechets_marque_hellinger,
  iterations = 5000,
  quietly = FALSE,
  status = TRUE,
  all = TRUE,
  cfa = FALSE,
  graph = TRUE,
  color = FALSE,
  col = c("black", "red", "blue"),
  lty = c(1, 2, 3),
  lwd = 1,
  legend = TRUE,
  file = "",
  width = 640,
  height = 640,
  grdevice = "png",
  seed = 0,
  mat = NA,
  n = NA
)

# Hopkins statistics
hopK <- hopkins::hopkins(macrodechets_marque_hellinger)
hopK # Close to 1
hopkins::hopkins.pval(hopK, dim(macrodechets_marque_hellinger)[1]) # significant pvalue

fviz_nbclust(marque_pca$x[, c(1:3)], FUNcluster = kmeans, k.max = 10) # 5 clusters
fviz_nbclust(marque_pca$x[, c(1:3)], FUNcluster = kmeans, k.max = 10, method = "wss") # 4 clusters


## PCA plotting ----
nice_pca_plot(marque_pca, axes = c(1, 2), nb_axes = 3, cluster_number = 4, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/marque/pca_marque_axes12.png",
    width = 2000,
    height = 1080,
    scale = 4,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

nice_pca_plot(marque_pca, axes = c(1, 3), nb_axes = 3, cluster_number = 4, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/marque/pca_marque_axes13.png",
    width = 2000,
    height = 1080,
    scale = 4,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

nice_pca_plot(marque_pca, axes = c(2, 3), nb_axes = 3, cluster_number = 4, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/marque/pca_marque_axes23.png",
    width = 2000,
    height = 1080,
    scale = 4,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )


effet_group_biplots(marque_pca, macrodechets_marque_habillage, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/marque/pca_marque_groups.png",
    width = 4000,
    height = 1000,
    scale = 3,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )

effet_group_biplots(marque_pca, macrodechets_marque_habillage, addEllipses = FALSE, contrib = 20) %>%
  ggsave(
    filename = "figures/pca/marque/pca_marque_groups_no_ellipses.png",
    width = 4000,
    height = 1000,
    scale = 3,
    units = "px",
    limitsize = FALSE,
    dpi = 300,
    bg = "white"
  )
