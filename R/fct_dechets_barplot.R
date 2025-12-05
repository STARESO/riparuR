#' ---
#' title : "RIPARU - dechet barplot function"
#' author : Aubin Woehrel
#' date : 2025-12-05
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - DECHETS BARPLOT FUNCTION
#'
#' Description :
#' Function for barplot representation of most common dechets
#'
#' =============================================================================

dechets_barplot <- function(
  dechets_data = NULL,
  subcat = NULL,
  save_name = NULL,
  scale_log = FALSE,
  width = 2000,
  height = 1200
) {
  # Missing checks
  if (is.null(dechets_data)) {
    stop("No data, please enter dataset with parameter dechet_data")
  }
  if (is.null(subcat)) {
    stop("No sub category specified, please enter the one selected")
  }

  # Scale log conditional changes
  if (scale_log) {
    dechets_data <- dechets_data %>%
      filter(total_100m > 1)

    save_name <- paste0(save_name, "_logscale")
  }

  # Barplot
  barplot_selected <- dechets_data %>%
    # filter(total_100m >= 10) %>%
    ggplot(., aes(
      x = reorder(print_name, -total_100m),
      y = total_100m,
      fill = level
    )) +
    geom_bar(stat = "identity") +
    paletteer::scale_fill_paletteer_c("grDevices::Heat") +
    # scale_fill_identity() +
    theme_pubr() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "none"
    ) +
    labs(x = "groupes", y = "abondance normalisée (sur 100m)")

  # If log scale
  if (scale_log) {
    barplot_selected <- barplot_selected +
      scale_y_log10()
  }

  # Output folder
  output_folder <- paste0(paths$output_mostcommon, subcat)

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }


  # Saving
  ggsave(
    plot = barplot_selected,
    width = width,
    height = height,
    scale = 3,
    filename = paste0(output_folder, "/", save_name, ".png"),
    units = "px",
    dpi = "print",
    limitsize = FALSE
  )

  return(paste0("Fichier exporté au chemin ", paste0(output_folder, "/", save_name)))
}
