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
  common_scale = FALSE,
  smooth = FALSE,
  span = 0.6,
  palette_selected = NULL,
  date_breaks = "3 months",
  date_limits = NULL,
  width = 2000,
  height = 1000 # 1200 if facet wrap on selected sites
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

  if (common_scale) {
    ymin <- 0
    ymax <- max(dechets_data$valeur_m2)
  }

  # season_label <- function(x) {
  #   m <- lubridate::month(x)
  #   y <- lubridate::year(x)

  #   season <- dplyr::case_when(
  #     m %in% 3:5 ~ "Printemps",
  #     m %in% 6:8 ~ "Été",
  #     m %in% 9:11 ~ "Automne",
  #     TRUE ~ "Hiver"
  #   )

  #   # Winter: Jan-Feb belong to previous year (winter started in Dec)
  #   season_year <- ifelse(season == "Hiver" & m %in% c(1, 2), y - 1, y)

  #   paste0(season_year, " – ", season)
  # }

  save_name_base <- save_name

  for (site_selected in sites_macrodechets) {
    save_name <- paste0(save_name_base, "_", site_selected)

    ggdechets <- dechets_data %>%
      filter(site == site_selected) %>%
      ggplot(., aes(x = date, y = valeur_m2, color = categorie_specifique, label = date))

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
      # facet_wrap(~site, scales = "free_y", axes = "all") +
      # scale_x_date(date_breaks = date_breaks, limits = date_limits, labels = season_label) +
      scale_x_date(date_breaks = date_breaks, limits = date_limits, date_labels = "%Y - %b") +
      scale_color_manual(name = str_to_upper(subcat), values = palette_selected) +
      theme_pubr() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25),
        legend.position = "right",
        axis.title.y = element_markdown()
      ) +
      labs(
        title = site_selected,
        x = "",
        y = "Nombre d'objets / m<sup>2</sup>"
      )

    # Log scale
    if (log_scale) {
      ggdechets <- ggdechets + scale_y_log10()
      save_name <- paste0(save_name, "_logscale")
    }

    if (common_scale) {
      ggdechets <- ggdechets + scale_y_continuous(limits = c(ymin, ymax))
      save_name <- paste0(save_name, "_commonscale")
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
      scale = 1.7,
      filename = paste0(output_folder, "/", save_name, "_", ".png"),
      units = "px",
      dpi = "print",
      limitsize = FALSE
    )
  }

  return(paste("Files saved at path ", output_folder))
}
