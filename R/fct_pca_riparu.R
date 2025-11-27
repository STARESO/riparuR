#' ---
#' title : "RIPARU - useful pca functions"
#' author : Aubin Woehrel
#' date : 2024-09-26
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - USEFUL PCA FUNCTIONS
#'
#' Description :
#' This code contains useful functions to perform PCA analysis specifically for
#' the MARE VIVU RIPARU macrodechets data.
#'
#' =============================================================================

library(dplyr)

# Loading macroplastics_data function for specific categorie_sub
macrodechets_selection <- function(macrodechets_counts, cat_choice, categorie_removal = NULL) {
  macrodechets_data <- macrodechets_counts %>%
    dplyr::filter(categorie_sub == cat_choice) %>%
    dplyr::mutate(id = paste(substr(date, 1, 4), substr(date, 6, 7), site, sep = "_")) %>%
    dplyr::select(id, categorie_specifique, valeur_100m) %>%
    tidyr::pivot_wider(names_from = categorie_specifique, values_from = valeur_100m) %>%
    tibble::column_to_rownames(var = "id") %>%
    dplyr::select(tidyselect::where(~ sum(., na.rm = TRUE) != 0)) %>%
    dplyr::select(!dplyr::all_of(categorie_removal)) %>%
    as.matrix()

  macrodechets_data
}

macrodechets_habillage <- function(macrodechets_counts, cat_choice) {
  macrodechets_habillage <- macrodechets_counts %>%
    filter(categorie_sub == categorie_sub) %>%
    dplyr::mutate(id = paste(substr(date, 1, 4), substr(date, 6, 7), site, sep = "_")) %>%
    dplyr::select("id", "annee", "saison", "site") %>%
    dplyr::distinct() %>%
    tibble::column_to_rownames("id")

  macrodechets_habillage
}


# Nicer pca function bundle than native fviz_pca_ functions ----
nice_pca_plot <- function(pr_comp_res, axes = c(1, 2), nb_axes = 2, cluster_number = 0,
                          midpoint = 10, contrib = NULL) {
  # Contribution of variables to principal components
  contrib_axis1 <- factoextra::fviz_contrib(pr_comp_res, "var", axes = axes[1], top = 10)
  contrib_axis2 <- factoextra::fviz_contrib(pr_comp_res, "var", axes = axes[2], top = 10)

  p1 <- factoextra::fviz_pca_var(
    pr_comp_res,
    axes = axes,
    repel = TRUE,
    col.var = "contrib",
    select.var = list(contrib = contrib)
  ) +
    ggplot2::scale_color_gradient2(low = "white", mid = "blue", high = "red", midpoint = midpoint, space = "Lab") +
    ggplot2::theme(legend.position = "bottom")


  if (cluster_number != 0) {
    clustering_pca <- factoextra::eclust(
      pr_comp_res$x[, 1:nb_axes],
      "kmeans",
      hc_metric = "eucliden",
      k = cluster_number
    )

    p2 <- factoextra::fviz_pca_ind(
      pr_comp_res,
      axes = axes,
      habillage = clustering_pca$cluster,
      addEllipses = TRUE,
      ellipse.level = 0.7,
      repel = TRUE
    )
  } else {
    p2 <- factoextra::fviz_pca_ind(pr_comp_res, axes = axes, repel = TRUE)
  }


  return(
    ggpubr::ggarrange(
      contrib_axis1,
      contrib_axis2,
      p1,
      p2,
      ncol = 2,
      nrow = 2,
      heights = axes
    )
  )
}


# Biplots for predicting parameters ----
effet_group_biplots <- function(
  res_comp,
  habillage_data,
  addEllipses = TRUE,
  contrib = NULL,
  ellipse.level = 0.7,
  repel = TRUE
) {
  p1 <- factoextra::fviz_pca_biplot(
    res_comp,
    repel = repel,
    # col.var = "cos2", # Variables color,
    addEllipses = addEllipses,
    ellipse.level = ellipse.level,
    col.var = "black",
    select.var = list(contrib = contrib),
    habillage = habillage_data[["site"]]
  )

  p2 <- factoextra::fviz_pca_biplot(
    res_comp,
    repel = repel,
    # col.var = "cos2", # Variables color,
    addEllipses = addEllipses,
    ellipse.level = ellipse.level,
    col.var = "black",
    select.var = list(contrib = contrib),
    habillage = habillage_data[["annee"]]
  )

  p3 <- factoextra::fviz_pca_biplot(
    res_comp,
    repel = repel,
    # col.var = "cos2", # Variables color,
    addEllipses = addEllipses,
    ellipse.level = ellipse.level,
    col.var = "black",
    select.var = list(contrib = contrib),
    habillage = habillage_data[["saison"]]
  )

  ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
}
