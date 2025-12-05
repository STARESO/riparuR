#' ---
#' title : "RIPARU - dechet treemap function"
#' author : Aubin Woehrel
#' date : 2025-12-05
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - DECHETS TREEMAP FUNCTION
#'
#' Description :
#' Function for treemap representation
#'
#' =============================================================================

dechets_treemap <- function(
  dechets_data = NULL,
  subcat = NULL,
  save_name = NULL,
  width = 1000,
  height = 1200
) {
  # Missing checks
  if (is.null(dechets_data)) {
    stop("No data, please enter dataset with parameter dechet_data")
  }
  if (is.null(subcat)) {
    stop("No sub category specified, please enter the one selected")
  }

  # Treemap
  treemap_selected <- dechets_data %>%
    ggplot(., aes(area = freq, fill = level, label = print_name)) +
    treemapify::geom_treemap(layout = "srow") +
    treemapify::geom_treemap_text(
      place = "centre",
      layout = "srow",
      size = 18,
      reflow = TRUE,
      color = "white"
    ) +
    labs(title = paste0("Proportion de déchets plastiques - ", subcat)) +
    paletteer::scale_fill_paletteer_c("grDevices::Heat") +
    theme_pubr() +
    theme(legend.position = "none", plot.title = element_text(size = 30))


  # Output folder
  output_folder <- paste0(paths$output_mostcommon, subcat)

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  # Saving
  ggsave(
    plot = treemap_selected,
    width = width,
    height = height,
    scale = 4,
    filename = paste0(output_folder, "/", save_name),
    units = "px",
    dpi = "print",
    limitsize = FALSE
  )
}
