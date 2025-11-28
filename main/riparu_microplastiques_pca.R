#' ---
#' title : "RIPARU - Microplastics PCA"
#' author : Aubin Woehrel
#' date : 2024-09-26
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - MICROPLASTICS PCA
#'
#' Description :
#' PCA analysis on the microplastics data.
#'
#' =============================================================================


# Initialization ----

## Clean up and working directory if needed-----
rm(list = ls())

setwd("C:/Users/aubin/Documents/Travail/Stareso/MareVivu/RIPARU/")

## Library imports -----

# Data imports
library(readxl)

# Plotting
library(ggplot2)
library(ggforce)

# Multivariate analysis
library(vegan)
library(FactoMineR)
library(factoextra)
library(paran)

# Maps
library(mapview)
library(sf)

# Data tidying 
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

## Data imports ----

microplastics <- readRDS("Analyses/OutputData/Riparu_microplastics.rds")
microplastics_total <- readRDS("Analyses/OutputData/Riparu_microplastics_total.rds")
macroplastics <- readRDS("Analyses/OutputData/Riparu_macroplastics.rds")
macroplastics_essentiel <- readRDS("Analyses/OutputData/Riparu_macroplastics_essentiel.rds")
typologie_sites <- readRDS("Analyses/OutputData/Riparu_typologie_sites.rds")


# Data tyding and preparation ----

# Transforming the microplastics dataset into a matrix with the total normalized microplastics (per 100m of coast)

microplastics <- microplastics %>%
  dplyr::select("Type", "Annee", "Saison", "Site", "Total_normalise") %>%
  # Fuse the Annee and Saison columns to create a unique identifier for each year and season
  mutate(id_ech = paste(Site, Annee, Saison, sep = "_"))

microplastics_matrix <- microplastics %>%
  dplyr::select(-c("Annee", "Saison", "Site")) %>%
  pivot_wider(names_from = Type, values_from = Total_normalise) %>%
  column_to_rownames("id_ech") %>%
  as.matrix()

# Using hellinger transformation due to many zeros
microplastics_matrix_h <- decostand(microplastics_matrix, method = "hellinger")

# Reference data for habillage plotting of ellipses :
microplastics_habillage <- microplastics %>%
  dplyr::select("Site", "Annee", "Saison", "id_ech") %>%
  distinct() %>%
  column_to_rownames("id_ech") 

# PCA ----

## PCA Computing and choices ----

# Compute the PCA
res_pca <- prcomp(microplastics_matrix_h, center = TRUE, scale. =  TRUE)

# Percentage of explained variance by dimensions of axes 
summary(res_pca)
fviz_eig(res_pca)

# Parallel analysis using paran package (Horn 1965) to determine the number of axes to retain

paran::paran(microplastics_matrix_h, 
             iterations = 10000,
             quietly = FALSE,
             status = TRUE, 
             all = TRUE, 
             cfa = FALSE, 
             graph = TRUE,
             color = FALSE, 
             col = c("black","red","blue"),
             lty = c(1,2,3), 
             lwd = 1, 
             legend = TRUE, 
             file = "",
             width = 640,
             height = 640, 
             grdevice = "png", 
             seed = 0, 
             mat = NA, 
             n = NA)

# Eigenvalues
eig.val <- get_eigenvalue(res_pca)
eig.val

# Hopkins statistics on the dataset to determine if it is suitable for clustering
hopK <- hopkins::hopkins(microplastics_matrix_h)
hopK # Close to 1 
hopkins::hopkins.pval(hopK, dim(microplastics_matrix_h)[1]) # significant pvalue

fviz_nbclust(res_pca$x[, c(1:3)], FUNcluster = kmeans, k.max = 10) # 2 clusters
fviz_nbclust(res_pca$x[, c(1:3)], FUNcluster = kmeans, k.max = 10, method = "wss") # 4 clusters
fviz_nbclust(res_pca$x[, c(1:3)], FUNcluster = kmeans, k.max = 10, method = "wss")


## Plotting the PCA ----

nice_pca_plot <- function(pr_comp_res, axes = c(1, 2), nb_axes = 2, cluster_number = 0, midpoint = 10) {
  
  # Contribution of variables to principal components
  contrib_axis1 <- fviz_contrib(pr_comp_res, "var", axes = axes[1], top = 10)
  contrib_axis2 <- fviz_contrib(pr_comp_res, "var", axes = axes[2], top = 10)
  
  p1 <- fviz_pca_var(pr_comp_res, axes = axes, repel = TRUE, col.var = "contrib") +
    scale_color_gradient2(low = "white", mid = "blue", high = "red", midpoint = midpoint, space = "Lab") +
    theme(legend.position = 'bottom')
  
  
  if (cluster_number != 0) {
    
    clustering_pca <- eclust(pr_comp_res$x[, 1:nb_axes], "kmeans", hc_metric = "eucliden", k = cluster_number)
    
    p2 <- fviz_pca_ind(pr_comp_res, axes = axes, habillage = clustering_pca$cluster,
                       addEllipses = TRUE, ellipse.level = 0.7, repel = TRUE)
    
  } else {
    
    p2 <- fviz_pca_ind(pr_comp_res, axes = axes, repel = TRUE)
    
  }
  
  
  return(ggpubr::ggarrange(contrib_axis1, contrib_axis2, p1, p2, ncol = 2, nrow = 2, heights = axes))
}




nice_pca_plot(res_pca, axes = c(1, 2), nb_axes = 3, cluster_number = 4) %>%
  ggsave(filename = "Analyses/Figures/PCA/PCA_Hellinger_axes12.png",
         width = 2000,
         height = 1080,
         scale = 4,
         units = "px",
         limitsize = FALSE,
         dpi = 300,
         bg = "white")

nice_pca_plot(res_pca, axes = c(1, 3), nb_axes = 3, cluster_number = 4) %>%
  ggsave(filename = "Analyses/Figures/PCA/PCA_Hellinger_axes13.png",
         width = 2000,
         height = 1080,
         scale = 4,
         units = "px",
         limitsize = FALSE,
         dpi = 300,
         bg = "white")


nice_pca_plot(res_pca, axes = c(2, 3), nb_axes = 3, cluster_number = 4) %>%
  ggsave(filename = "Analyses/Figures/PCA/PCA_Hellinger_axes23.png",
         width = 2000,
         height = 1080,
         scale = 4,
         units = "px",
         limitsize = FALSE,
         dpi = 300,
         bg = "white")


## Again with removal of outliers ----

# Remove the row names Macinaghju_2023_Printemps and Petracurbara_2023_Printemps from microplastics matrix

remove_outliers <- function(matrix, outliers) {
  
  return(matrix[!rownames(matrix) %in% outliers, ])
  
}

microplastics_matrix_h_no_outliers <- remove_outliers(microplastics_matrix_h, 
                                                      c("Macinaghju_2023_Printemps", "Petracurbara_2023_Printemps"))

microplastics_habillage_no_outliers <- microplastics_habillage[!rownames(microplastics_habillage) %in% 
                                                               c("Macinaghju_2023_Printemps", "Petracurbara_2023_Printemps"), ]


# Compute the PCA
res_pca <- prcomp(microplastics_matrix_h_no_outliers, center = TRUE, scale. =  TRUE)

# Percentage of explained variance by dimensions of axes 
summary(res_pca)
fviz_eig(res_pca)

# Parallel analysis using paran package (Horn 1965) to determine the number of axes to retain

paran::paran(microplastics_matrix_h_no_outliers, 
             iterations = 10000,
             quietly = FALSE,
             status = TRUE, 
             all = TRUE, 
             cfa = FALSE, 
             graph = TRUE,
             color = FALSE, 
             col = c("black","red","blue"),
             lty = c(1,2,3), 
             lwd = 1, 
             legend = TRUE, 
             file = "",
             width = 640,
             height = 640, 
             grdevice = "png", 
             seed = 0, 
             mat = NA, 
             n = NA)

# Eigenvalues
eig.val <- get_eigenvalue(res_pca)
eig.val

# Hopkins statistics on the dataset to determine if it is suitable for clustering
hopK <- hopkins::hopkins(microplastics_matrix_h)
hopK # Close to 1 
hopkins::hopkins.pval(hopK, dim(microplastics_matrix_h)[1]) # significant pvalue

fviz_nbclust(res_pca$x[, c(1:2)], FUNcluster = kmeans, k.max = 10) # 2 clusters
fviz_nbclust(res_pca$x[, c(1:2)], FUNcluster = kmeans, k.max = 10, method = "wss") # 4 clusters
fviz_nbclust(res_pca$x[, c(1:2)], FUNcluster = kmeans, k.max = 10, method = "wss")

nice_pca_plot(res_pca, axes = c(1, 2), nb_axes = 2, cluster_number = 4) %>%
  ggsave(filename = "Analyses/Figures/PCA/PCA_Hellinger_no_outliers.png",
         width = 2000,
         height = 1080,
         scale = 4,
         units = "px",
         limitsize = FALSE,
         dpi = 300,
         bg = "white")


# Plot this and save it with ggsave like before
clutering_outliers <- eclust(res_pca$x[, 1:2], "kmeans", hc_metric = "eucliden", k = 4)  # Individuals color
fviz_pca_biplot(res_pca, repel = TRUE,
                #col.var = "cos2", # Variables color,
                habillage = clutering_outliers$cluster) %>%
  #scale_color_brewer(palette = "Dark2") %>%
ggsave(filename = "Analyses/Figures/PCA/PCA_Hellinger_no_outliers_biplot.png",
       width = 2000,
       height = 1500,
       scale = 2,
       units = "px",
       limitsize = FALSE,
       dpi = 300,
       bg = "white")

# Create ellipses representation with fviz_ellipse by the variables Site, Annee and Saison 
# contained in the name of the column corresponding to each observation

effet_group_biplots <- function(res_comp, addEllipses = TRUE, ellipse.level = 0.7, repel = TRUE) {
  
  p1 <- fviz_pca_biplot(res_comp, repel = repel,
                  #col.var = "cos2", # Variables color,
                  addEllipses = addEllipses, ellipse.level = ellipse.level,
                  habillage = microplastics_habillage_no_outliers$Site) 
  
  p2 <- fviz_pca_biplot(res_comp, repel = repel,
                  #col.var = "cos2", # Variables color,
                  addEllipses = addEllipses, ellipse.level = ellipse.level,
                  habillage = microplastics_habillage_no_outliers$Annee) 
  
  p3 <- fviz_pca_biplot(res_comp, repel = repel,
                  #col.var = "cos2", # Variables color,
                  addEllipses = addEllipses, ellipse.level = ellipse.level,
                  habillage = microplastics_habillage_no_outliers$Saison)
  
  ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
  
}

effet_group_biplots(res_pca) %>%
  ggsave(filename = "Analyses/Figures/PCA/PCA_Hellinger_no_outliers_groups_ellipses.png",
         width = 4000,
         height = 1500,
         scale = 3,
         units = "px",
         limitsize = FALSE,
         dpi = 300,
         bg = "white")

effet_group_biplots(res_pca, addEllipses = FALSE) %>%
  ggsave(filename = "Analyses/Figures/PCA/PCA_Hellinger_no_outliers_groups_no_ellipses.png",
         width = 4000,
         height = 1500,
         scale = 3,
         units = "px",
         limitsize = FALSE,
         dpi = 300,
         bg = "white")





