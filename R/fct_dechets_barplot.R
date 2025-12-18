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
      filter(total_m2 > 1)

    save_name <- paste0(save_name, "_logscale")
  }

  # Barplot
  barplot_selected <- dechets_data %>%
    filter(level < 50) %>%
    mutate(level_fac = factor(level, levels = 1:50)) %>%
    # filter(total_m2 >= 10) %>%
    ggplot(., aes(
      # x = reorder(print_name, -total_m2),
      x = level_fac,
      y = total_m2,
      fill = level
    )) +
    geom_bar(stat = "identity") +
    paletteer::scale_fill_paletteer_c("grDevices::Heat") +
    # scale_fill_identity() +
    theme_pubr() +
    theme(
      # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "none",
      axis.title.y = element_markdown()
    ) +
    labs(x = "Classement de groupe (cf. référence tabulaire)", y = "Nombre d'objets / m<sup>2</sup>")

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


  # Saving barplot
  ggsave(
    plot = barplot_selected,
    width = width,
    height = height,
    scale = 2,
    filename = paste0(output_folder, "/", save_name, ".png"),
    units = "px",
    dpi = "print",
    limitsize = FALSE
  )


  # Saving associated number-level and category reference
  dechets_data %>%
    select(print_name, total_m2) %>%
    openxlsx::write.xlsx(., paste0(output_folder, "/", save_name, "reference.xlsx"))


  return(paste0("File saved at path ", paste0(output_folder, "/", save_name)))
}
