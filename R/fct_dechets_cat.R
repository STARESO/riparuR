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
dechets_cat <- function(
  categorie_sub_sel, # selected sub category (ex : REP, Marque, ...)
  sites_selected = NULL, # In case of specific list of sites
  categorie_spe_sel = NULL, # selection of specific categories
  sum_by_cat = TRUE # For summarizing data per
) {
  # Conditional checks
  if (is.null(sites_selected)) {
    stop("No site selection defined, please choose 'all' or selected sites")
  }

  if (is.null(categorie_sub_sel)) {
    stop("No category selected, please select one")
  }

  # Selection of sites
  if (length(sites_selected) == 1) {
    if (sites_selected == "all") {
      macrodechets_selected <- macrodechets_nb
    }
  } else {
    macrodechets_selected <- macrodechets_nb %>%
      filter(site %in% sites_selected)
  }

  # Filtering selected sub category
  macrodechets_selected <- macrodechets_selected %>%
    filter(categorie_sub == categorie_sub_sel)

  # In case of specific category selection
  if (!is.null(categorie_spe_sel)) {
    macrodechets_selected <- macrodechets_selected %>%
      filter(categorie_specifique %in% categorie_spe_sel)
  }

  # In case of summarizing per date for each specific category
  if (sum_by_cat) {
    macrodechets_selected <- macrodechets_selected %>%
      group_by(categorie_specifique) %>%
      summarize(total_m2 = sum(valeur_m2, na.rm = TRUE)) %>%
      # mutate(total_m2 = round(total_m2, 2)) %>%
      arrange(desc(total_m2)) %>%
      filter(total_m2 > 0) %>%
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

    total_selected <- sum(macrodechets_selected$total_m2)
    macrodechets_selected <- macrodechets_selected %>%
      dplyr::mutate(freq = total_m2 / total_selected)
  } else { # Condition of keeping observations per date
    macrodechets_selected <- macrodechets_selected %>%
      mutate(categorie_specifique = paste(str_replace_all(categorie_specifique, "_", " ")))
  }

  return(macrodechets_selected)
}
