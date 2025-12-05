#' ---
#' title : "RIPARU - dechets time series function"
#' author : Aubin Woehrel
#' date : 2025-12-05
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - DECHETS TIMESERIES FUNCTION
#'
#' Description :
#' Time series plotting of macrodechets
#'
#' =============================================================================


dechets_timeseries <- function(
  dechets_data = NULL,
  subcat = NULL,
  save_name = NULL,
  points = TRUE,
  log_scale = FALSE,
  smooth = FALSE,
  span = 0.6,
  palette_selected = NULL,
  date_breaks = "3 months",
  date_limits = NULL,
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

  # Date limits
  if (is.null(date_limits)) {
    warning("Date limits are not set. Using predefined date limits.")
    date_limits <- c(
      as.Date("2022-01-01"),
      as.Date(paste0(max(dechets_data$annee)), "-12-31")
    )
  }

  ggdechets <- ggplot(dechets_data, aes(x = date, y = valeur_100m, color = categorie_specifique))

  # Showing points
  if (points) {
    ggdechets <- ggdechets + geom_point()
  }

  # If smoothing wanted
  if (smooth) {
    ggdechets <- ggdechets + geom_smooth(se = FALSE, span = span)
    save_name <- paste0(save_name, "smooth_span", span * 100)
  } else {
    ggdechets <- ggdechets + geom_line()
  }

  # Rest of plot
  ggdechets <- ggdechets +
    facet_wrap(~site, scales = "free_y", axes = "all") +
    scale_x_date(date_breaks = date_breaks, limits = date_limits) +
    scale_color_manual(values = palette_selected) +
    theme_pubr() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "right"
    ) +
    labs(
      title = "Nombre de macrodéchets trouvés par catégorie spécifique",
      x = "",
      y = "Nombre d'objets / 100 mètres"
    )

  # Log scale
  if (log_scale) {
    ggdechets <- ggdechets + scale_y_log10()
    save_name <- paste0(save_name, "_logscale")
  }

  # Output folder
  output_folder <- paste0(paths$output_timeseries, subcat)

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  # Saving
  ggsave(
    plot = ggdechets,
    width = width,
    height = height,
    scale = 3.5,
    filename = paste0(output_folder, "/", save_name, ".png"),
    units = "px",
    dpi = "print",
    limitsize = FALSE
  )

  return(paste0("File saved at path ", paste0(output_folder, "/", save_name, ".png")))
}
