#' ---
#' title : "RIPARU - dechets cat function"
#' author : Aubin Woehrel
#' date : 2025-12-05
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - DECHETS CAT FUNCTION
#'
#' Description :
#' Function for preparing data of macrodechets with specific sub-group
#' for plotting
#'
#' =============================================================================

#' Main function ----
dechets_cat <- function(categorie_sub_sel, sites_selected = NULL) {
  if (is.null(sites_selected)) {
    stop("No site selection defined, please choose 'all' or selected sites")
  }

  if (is.null(categorie_sub_sel)) {
    stop("No category selected, please select one")
  }

  if (length(sites_selected) == 1) {
    if (sites_selected == "all") {
      macrodechets_selected <- macrodechets_nb
    }
  } else {
    macrodechets_selected <- macrodechets_nb %>%
      filter(site %in% sites_selected)
  }

  macrodechets_selected <- macrodechets_selected %>%
    filter(categorie_sub == categorie_sub_sel) %>%
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

  total_selected <- sum(macrodechets_selected$total_100m)

  macrodechets_selected <- macrodechets_selected %>%
    dplyr::mutate(freq = total_100m / total_selected)
}
